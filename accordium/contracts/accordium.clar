;; Multi-Party Escrow System
;; Secure transactions between buyer, seller, and arbiter with comprehensive dispute resolution

;; <CHANGE> Renamed error constants to consistent error-* format
(define-constant error-not-found (err u404))
(define-constant error-access-denied (err u401))
(define-constant error-invalid-state (err u400))
(define-constant error-invalid-params (err u422))
(define-constant error-already-exists (err u409))
(define-constant error-insufficient-balance (err u402))
(define-constant error-expired (err u408))
(define-constant error-contract-paused (err u503))
(define-constant error-transfer-failed (err u500))
(define-constant error-invalid-percentage (err u423))
(define-constant error-not-active (err u424))

;; <CHANGE> Renamed contract constants to lowercase format with descriptive names
(define-constant contract-owner tx-sender)
(define-constant maximum-fee-percentage u1000)    ;; 10%
(define-constant maximum-duration-blocks u52560)   ;; ~1 year in blocks
(define-constant minimum-escrow-amount u1000)      ;; Minimum escrow amount (0.001 STX)
(define-constant maximum-protocol-fee-percent u500) ;; 5%
(define-constant minimum-reason-length u10)
(define-constant maximum-reason-length u256)
(define-constant maximum-name-length u64)
(define-constant precision-scale u10000) ;; 100.00%

;; <CHANGE> Renamed data variables to consistent lowercase format
(define-data-var next-escrow-id uint u1)
(define-data-var protocol-fee-rate uint u50)  ;; 0.5%
(define-data-var fee-collector-address principal contract-owner)
(define-data-var contract-paused bool false)
(define-data-var total-escrows-created uint u0)
(define-data-var total-volume-processed uint u0)
(define-data-var emergency-mode-active bool false)

;; Main escrow data structure
(define-map escrow-registry
  { id: uint }
  {
    creator: principal,
    buyer: principal,
    seller: principal,
    arbiter: principal,
    amount: uint,
    status: (string-ascii 16),
    created-at: uint,
    expires-at: (optional uint),
    buyer-confirmed: bool,
    seller-confirmed: bool,
    funded-amount: uint,
    dispute-reason: (optional (string-utf8 256)),
    last-activity: uint,
    metadata: (optional (string-utf8 128))
  }
)

;; Deposits tracking for audit trail
(define-map deposit-log
  { escrow-id: uint }
  { 
    depositor: principal, 
    amount: uint, 
    timestamp: uint,
    block-height: uint
  }
)

;; Arbiters registry with reputation system
(define-map arbiter-registry
  { arbiter: principal }
  {
    name: (string-utf8 64),
    fee-rate: uint,
    disputes-resolved: uint,
    disputes-won-buyer: uint,
    disputes-won-seller: uint,
    active: bool,
    registered-at: uint,
    last-activity: uint,
    reputation-score: uint
  }
)

;; Escrow participants for quick lookups
(define-map participant-registry
  { escrow-id: uint, participant: principal }
  { role: (string-ascii 16), joined-at: uint }
)

;; Transaction history for transparency
(define-map history-log
  { escrow-id: uint, sequence: uint }
  {
    action: (string-ascii 32),
    actor: principal,
    timestamp: uint,
    block-height: uint,
    details: (optional (string-utf8 256))
  }
)

;; Transaction sequence tracking
(define-map sequence-tracker
  { escrow-id: uint }
  { next-sequence: uint }
)

;; Helper functions for validation and calculations
(define-private (validate-amount (amount uint))
  (and (> amount u0) (>= amount minimum-escrow-amount))
)

(define-private (validate-principal (user principal))
  (not (is-eq user (as-contract tx-sender)))
)

(define-private (check-participant (user principal) (buyer principal) (seller principal))
  (or (is-eq user buyer) (is-eq user seller))
)

(define-private (check-contract-paused)
  (var-get contract-paused)
)

(define-private (check-emergency-mode)
  (var-get emergency-mode-active)
)

(define-private (calc-protocol-fee (amount uint))
  (/ (* amount (var-get protocol-fee-rate)) precision-scale)
)

(define-private (calc-arbiter-fee (amount uint) (fee-rate uint))
  (/ (* amount fee-rate) precision-scale)
)

(define-private (check-escrow-expired (escrow-data (tuple (creator principal) (buyer principal) (seller principal) (arbiter principal) (amount uint) (status (string-ascii 16)) (created-at uint) (expires-at (optional uint)) (buyer-confirmed bool) (seller-confirmed bool) (funded-amount uint) (dispute-reason (optional (string-utf8 256))) (last-activity uint) (metadata (optional (string-utf8 128))))))
  (match (get expires-at escrow-data)
    expiry (> block-height expiry)
    false
  )
)

(define-private (validate-dispute-reason (reason (string-utf8 256)))
  (let ((reason-length (len reason)))
    (and 
      (>= reason-length minimum-reason-length)
      (<= reason-length maximum-reason-length)
    )
  )
)

;; Fixed log-transaction function to properly track sequence numbers
(define-private (log-transaction (escrow-id uint) (action (string-ascii 32)) (details (optional (string-utf8 256))))
  (let ((current-seq-data (default-to { next-sequence: u1 } (map-get? sequence-tracker { escrow-id: escrow-id })))
        (sequence (get next-sequence current-seq-data)))
    
    ;; Update sequence counter
    (map-set sequence-tracker
      { escrow-id: escrow-id }
      { next-sequence: (+ sequence u1) }
    )
    
    ;; Add transaction record
    (map-set history-log
      { escrow-id: escrow-id, sequence: sequence }
      {
        action: action,
        actor: tx-sender,
        timestamp: block-height,
        block-height: block-height,
        details: details
      }
    )
  )
)

;; Create escrow with enhanced validation
(define-public (new-escrow
  (buyer principal)
  (seller principal)
  (arbiter principal)
  (amount uint)
  (duration-blocks (optional uint))
  (metadata (optional (string-utf8 128))))
  
  (let ((escrow-id (var-get next-escrow-id)))
    
    ;; Basic validations
    (asserts! (not (check-contract-paused)) error-contract-paused)
    (asserts! (not (check-emergency-mode)) error-contract-paused)
    (asserts! (validate-amount amount) error-invalid-params)
    (asserts! (validate-principal buyer) error-invalid-params)
    (asserts! (validate-principal seller) error-invalid-params)
    (asserts! (validate-principal arbiter) error-invalid-params)
    (asserts! (not (is-eq buyer seller)) error-invalid-params)
    (asserts! (not (is-eq buyer arbiter)) error-invalid-params)
    (asserts! (not (is-eq seller arbiter)) error-invalid-params)
    
    ;; Validate arbiter exists and is active
    (let ((arbiter-data (unwrap! (map-get? arbiter-registry { arbiter: arbiter }) error-not-found)))
      (asserts! (get active arbiter-data) error-access-denied)
      
      ;; Validate duration if provided
      (match duration-blocks
        blocks (asserts! (and (> blocks u0) (<= blocks maximum-duration-blocks)) error-invalid-params)
        true
      )
      
      ;; Calculate expiration
      (let ((expiry (match duration-blocks
                      blocks (some (+ block-height blocks))
                      none)))
        
        ;; Create escrow record
        (map-set escrow-registry
          { id: escrow-id }
          {
            creator: tx-sender,
            buyer: buyer,
            seller: seller,
            arbiter: arbiter,
            amount: amount,
            status: "created",
            created-at: block-height,
            expires-at: expiry,
            buyer-confirmed: false,
            seller-confirmed: false,
            funded-amount: u0,
            dispute-reason: none,
            last-activity: block-height,
            metadata: metadata
          }
        )
        
        ;; Add participants
        (map-set participant-registry { escrow-id: escrow-id, participant: buyer } { role: "buyer", joined-at: block-height })
        (map-set participant-registry { escrow-id: escrow-id, participant: seller } { role: "seller", joined-at: block-height })
        (map-set participant-registry { escrow-id: escrow-id, participant: arbiter } { role: "arbiter", joined-at: block-height })
        
        ;; Add transaction record
        (log-transaction escrow-id "created" metadata)
        
        ;; Update counters
        (var-set next-escrow-id (+ escrow-id u1))
        (var-set total-escrows-created (+ (var-get total-escrows-created) u1))
        
        (ok escrow-id)
      )
    )
  )
)

;; Register as arbiter with enhanced profile
(define-public (join-arbiters (name (string-utf8 64)) (fee-rate uint))
  (begin
    (asserts! (not (check-contract-paused)) error-contract-paused)
    (asserts! (<= fee-rate maximum-fee-percentage) error-invalid-params)
    (asserts! (> (len name) u0) error-invalid-params)
    (asserts! (<= (len name) maximum-name-length) error-invalid-params)
    (asserts! (is-none (map-get? arbiter-registry { arbiter: tx-sender })) error-already-exists)
    (asserts! (validate-principal tx-sender) error-invalid-params)
    
    (map-set arbiter-registry
      { arbiter: tx-sender }
      {
        name: name,
        fee-rate: fee-rate,
        disputes-resolved: u0,
        disputes-won-buyer: u0,
        disputes-won-seller: u0,
        active: true,
        registered-at: block-height,
        last-activity: block-height,
        reputation-score: u5000  ;; Start with neutral reputation (50%)
      }
    )
    
    (ok true)
  )
)

;; Update arbiter profile
(define-public (update-profile (name (optional (string-utf8 64))) (fee-rate (optional uint)))
  (let ((arbiter-data (unwrap! (map-get? arbiter-registry { arbiter: tx-sender }) error-not-found)))
    
    ;; Validate new fee rate if provided
    (match fee-rate
      new-rate (asserts! (<= new-rate maximum-fee-percentage) error-invalid-params)
      true
    )
    
    ;; Validate new name if provided
    (match name
      new-name (begin
        (asserts! (> (len new-name) u0) error-invalid-params)
        (asserts! (<= (len new-name) maximum-name-length) error-invalid-params)
      )
      true
    )
    
    (map-set arbiter-registry
      { arbiter: tx-sender }
      (merge arbiter-data {
        name: (default-to (get name arbiter-data) name),
        fee-rate: (default-to (get fee-rate arbiter-data) fee-rate),
        last-activity: block-height
      })
    )
    
    (ok true)
  )
)

;; Toggle arbiter status
(define-public (toggle-status)
  (let ((arbiter-data (unwrap! (map-get? arbiter-registry { arbiter: tx-sender }) error-not-found)))
    (map-set arbiter-registry
      { arbiter: tx-sender }
      (merge arbiter-data { 
        active: (not (get active arbiter-data)),
        last-activity: block-height
      })
    )
    (ok true)
  )
)

;; Fund escrow with STX
(define-public (deposit-funds (escrow-id uint))
  (let ((escrow-data (unwrap! (map-get? escrow-registry { id: escrow-id }) error-not-found)))
    
    ;; Validations
    (asserts! (not (check-contract-paused)) error-contract-paused)
    (asserts! (is-eq (get status escrow-data) "created") error-invalid-state)
    (asserts! (check-participant tx-sender (get buyer escrow-data) (get seller escrow-data)) error-access-denied)
    (asserts! (is-eq (get funded-amount escrow-data) u0) error-already-exists)
    (asserts! (not (check-escrow-expired escrow-data)) error-expired)
    
    ;; Transfer STX to contract
    (try! (stx-transfer? (get amount escrow-data) tx-sender (as-contract tx-sender)))
    
    ;; Record deposit
    (map-set deposit-log
      { escrow-id: escrow-id }
      { 
        depositor: tx-sender, 
        amount: (get amount escrow-data), 
        timestamp: block-height,
        block-height: block-height
      }
    )
    
    ;; Update escrow status
    (map-set escrow-registry
      { id: escrow-id }
      (merge escrow-data { 
        status: "funded", 
        funded-amount: (get amount escrow-data),
        last-activity: block-height
      })
    )
    
    ;; Add transaction record
    (log-transaction escrow-id "funded" none)
    
    ;; Update total volume
    (var-set total-volume-processed (+ (var-get total-volume-processed) (get amount escrow-data)))
    
    (ok true)
  )
)

;; Confirm completion by buyer or seller
(define-public (mark-complete (escrow-id uint))
  (let ((escrow-data (unwrap! (map-get? escrow-registry { id: escrow-id }) error-not-found)))
    
    ;; Validations
    (asserts! (not (check-contract-paused)) error-contract-paused)
    (asserts! (is-eq (get status escrow-data) "funded") error-invalid-state)
    (asserts! (check-participant tx-sender (get buyer escrow-data) (get seller escrow-data)) error-access-denied)
    (asserts! (not (check-escrow-expired escrow-data)) error-expired)
    
    ;; Update confirmation status
    (let ((updated-escrow 
            (if (is-eq tx-sender (get buyer escrow-data))
                (merge escrow-data { buyer-confirmed: true, last-activity: block-height })
                (merge escrow-data { seller-confirmed: true, last-activity: block-height }))))
      
      (map-set escrow-registry { id: escrow-id } updated-escrow)
      
      ;; Add transaction record
      (log-transaction escrow-id "confirmed" none)
      
      ;; Check if both confirmed and release funds
      (if (and (get buyer-confirmed updated-escrow) (get seller-confirmed updated-escrow))
          (begin
            (map-set escrow-registry { id: escrow-id } (merge updated-escrow { status: "completed" }))
            (try! (payout-seller escrow-id))
            (ok true)
          )
          (ok true)
      )
    )
  )
)

;; Release funds to seller (private function)
(define-private (payout-seller (escrow-id uint))
  (let ((escrow-data (unwrap! (map-get? escrow-registry { id: escrow-id }) error-not-found))
        (arbiter-data (unwrap! (map-get? arbiter-registry { arbiter: (get arbiter escrow-data) }) error-not-found)))
    
    (let ((total-amount (get funded-amount escrow-data))
          (protocol-fee-amount (calc-protocol-fee total-amount))
          (arbiter-fee-amount (calc-arbiter-fee total-amount (get fee-rate arbiter-data)))
          (seller-payout-amount (- total-amount protocol-fee-amount arbiter-fee-amount)))
      
      ;; Ensure we have enough funds
      (asserts! (>= total-amount (+ protocol-fee-amount arbiter-fee-amount)) error-insufficient-balance)
      
      ;; Transfer protocol fee if > 0
      (if (> protocol-fee-amount u0)
          (try! (as-contract (stx-transfer? protocol-fee-amount tx-sender (var-get fee-collector-address))))
          true
      )
      
      ;; Transfer arbiter fee if > 0
      (if (> arbiter-fee-amount u0)
          (try! (as-contract (stx-transfer? arbiter-fee-amount tx-sender (get arbiter escrow-data))))
          true
      )
      
      ;; Transfer remaining to seller if > 0
      (if (> seller-payout-amount u0)
          (try! (as-contract (stx-transfer? seller-payout-amount tx-sender (get seller escrow-data))))
          true
      )
      
      ;; Add transaction record
      (log-transaction escrow-id "completed" none)
      
      (ok true)
    )
  )
)

;; File dispute with detailed reason
(define-public (raise-dispute (escrow-id uint) (reason (string-utf8 256)))
  (let ((escrow-data (unwrap! (map-get? escrow-registry { id: escrow-id }) error-not-found)))
    
    ;; Validations
    (asserts! (not (check-contract-paused)) error-contract-paused)
    (asserts! (is-eq (get status escrow-data) "funded") error-invalid-state)
    (asserts! (check-participant tx-sender (get buyer escrow-data) (get seller escrow-data)) error-access-denied)
    (asserts! (validate-dispute-reason reason) error-invalid-params)
    (asserts! (not (check-escrow-expired escrow-data)) error-expired)
    
    ;; Update escrow with dispute
    (map-set escrow-registry
      { id: escrow-id }
      (merge escrow-data { 
        status: "disputed", 
        dispute-reason: (some reason),
        last-activity: block-height
      })
    )
    
    ;; Add transaction record
    (log-transaction escrow-id "disputed" (some reason))
    
    (ok true)
  )
)

;; Resolve dispute (arbiter only) with percentage-based distribution
(define-public (settle-dispute (escrow-id uint) (buyer-percentage uint))
  (let ((escrow-data (unwrap! (map-get? escrow-registry { id: escrow-id }) error-not-found))
        (arbiter-data (unwrap! (map-get? arbiter-registry { arbiter: tx-sender }) error-not-found)))
    
    ;; Validations
    (asserts! (not (check-contract-paused)) error-contract-paused)
    (asserts! (is-eq tx-sender (get arbiter escrow-data)) error-access-denied)
    (asserts! (is-eq (get status escrow-data) "disputed") error-invalid-state)
    (asserts! (<= buyer-percentage precision-scale) error-invalid-percentage)
    
    ;; Calculate distribution
    (let ((total-amount (get funded-amount escrow-data))
          (protocol-fee-amount (calc-protocol-fee total-amount))
          (arbiter-fee-amount (calc-arbiter-fee total-amount (get fee-rate arbiter-data)))
          (remaining-amount (- total-amount protocol-fee-amount arbiter-fee-amount))
          (buyer-payout-amount (/ (* remaining-amount buyer-percentage) precision-scale))
          (seller-payout-amount (- remaining-amount buyer-payout-amount)))
      
      ;; Ensure valid amounts
      (asserts! (>= total-amount (+ protocol-fee-amount arbiter-fee-amount)) error-insufficient-balance)
      
      ;; Distribute funds
      ;; Protocol fee
      (if (> protocol-fee-amount u0)
          (try! (as-contract (stx-transfer? protocol-fee-amount tx-sender (var-get fee-collector-address))))
          true
      )
      
      ;; Arbiter fee
      (if (> arbiter-fee-amount u0)
          (try! (as-contract (stx-transfer? arbiter-fee-amount tx-sender tx-sender)))
          true
      )
      
      ;; Buyer share
      (if (> buyer-payout-amount u0)
          (try! (as-contract (stx-transfer? buyer-payout-amount tx-sender (get buyer escrow-data))))
          true
      )
      
      ;; Seller share  
      (if (> seller-payout-amount u0)
          (try! (as-contract (stx-transfer? seller-payout-amount tx-sender (get seller escrow-data))))
          true
      )
      
      ;; Update escrow status
      (map-set escrow-registry { id: escrow-id } (merge escrow-data { status: "resolved", last-activity: block-height }))
      
      ;; Update arbiter stats
      (let ((disputes-won-buyer-count (if (> buyer-percentage u5000) (+ (get disputes-won-buyer arbiter-data) u1) (get disputes-won-buyer arbiter-data)))
            (disputes-won-seller-count (if (<= buyer-percentage u5000) (+ (get disputes-won-seller arbiter-data) u1) (get disputes-won-seller arbiter-data))))
        
        (map-set arbiter-registry 
          { arbiter: tx-sender }
          (merge arbiter-data { 
            disputes-resolved: (+ (get disputes-resolved arbiter-data) u1),
            disputes-won-buyer: disputes-won-buyer-count,
            disputes-won-seller: disputes-won-seller-count,
            last-activity: block-height
          })
        )
      )
      
      ;; Add transaction record
      (log-transaction escrow-id "resolved" none)
      
      (ok true)
    )
  )
)

;; Refund escrow (if expired or unfunded cancellation)
(define-public (process-refund (escrow-id uint))
  (let ((escrow-data (unwrap! (map-get? escrow-registry { id: escrow-id }) error-not-found)))
    
    ;; Check refund conditions
    (let ((is-expired (check-escrow-expired escrow-data))
          (is-unfunded (is-eq (get status escrow-data) "created"))
          (is-creator (is-eq tx-sender (get creator escrow-data))))
      
      (asserts! (or is-expired (and is-unfunded is-creator)) error-invalid-state)
      
      ;; If funded, refund the depositor
      (if (> (get funded-amount escrow-data) u0)
          (let ((deposit-data (unwrap! (map-get? deposit-log { escrow-id: escrow-id }) error-not-found)))
            (try! (as-contract (stx-transfer? (get amount deposit-data) tx-sender (get depositor deposit-data))))
          )
          true
      )
      
      ;; Update status
      (map-set escrow-registry { id: escrow-id } (merge escrow-data { status: "refunded", last-activity: block-height }))
      
      ;; Add transaction record
      (log-transaction escrow-id "refunded" none)
      
      (ok true)
    )
  )
)

;; Emergency functions (contract owner only)
(define-public (trigger-emergency)
  (begin
    (asserts! (is-eq tx-sender contract-owner) error-access-denied)
    (var-set emergency-mode-active true)
    (var-set contract-paused true)
    (ok true)
  )
)

(define-public (clear-emergency)
  (begin
    (asserts! (is-eq tx-sender contract-owner) error-access-denied)
    (var-set emergency-mode-active false)
    (var-set contract-paused false)
    (ok true)
  )
)

;; Admin functions
(define-public (set-paused)
  (begin
    (asserts! (is-eq tx-sender contract-owner) error-access-denied)
    (var-set contract-paused true)
    (ok true)
  )
)

(define-public (clear-paused)
  (begin
    (asserts! (is-eq tx-sender contract-owner) error-access-denied)
    (asserts! (not (check-emergency-mode)) error-contract-paused)
    (var-set contract-paused false)
    (ok true)
  )
)

(define-public (set-system-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) error-access-denied)
    (asserts! (<= new-fee maximum-protocol-fee-percent) error-invalid-params)
    (var-set protocol-fee-rate new-fee)
    (ok true)
  )
)

(define-public (set-fee-collector (new-recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) error-access-denied)
    (asserts! (validate-principal new-recipient) error-invalid-params)
    (var-set fee-collector-address new-recipient)
    (ok true)
  )
)

;; Read-only functions for querying contract state
(define-read-only (fetch-escrow (escrow-id uint))
  (map-get? escrow-registry { id: escrow-id })
)

(define-read-only (fetch-arbiter (arbiter principal))
  (map-get? arbiter-registry { arbiter: arbiter })
)

(define-read-only (fetch-deposit (escrow-id uint))
  (map-get? deposit-log { escrow-id: escrow-id })
)

(define-read-only (fetch-role (escrow-id uint) (participant principal))
  (map-get? participant-registry { escrow-id: escrow-id, participant: participant })
)

(define-read-only (fetch-history (escrow-id uint) (sequence uint))
  (map-get? history-log { escrow-id: escrow-id, sequence: sequence })
)

(define-read-only (fetch-info)
  {
    paused: (var-get contract-paused),
    emergency-mode: (var-get emergency-mode-active),
    protocol-fee: (var-get protocol-fee-rate),
    fee-recipient: (var-get fee-collector-address),
    next-escrow-id: (var-get next-escrow-id),
    total-escrows-created: (var-get total-escrows-created),
    total-volume: (var-get total-volume-processed),
    owner: contract-owner
  }
)

(define-read-only (fetch-system-fee)
  (var-get protocol-fee-rate)
)

(define-read-only (fetch-counter)
  (var-get next-escrow-id)
)

(define-read-only (fetch-stats)
  {
    total-escrows: (var-get total-escrows-created),
    total-volume: (var-get total-volume-processed),
    protocol-fee-rate: (var-get protocol-fee-rate),
    is-paused: (var-get contract-paused),
    is-emergency: (var-get emergency-mode-active)
  }
)