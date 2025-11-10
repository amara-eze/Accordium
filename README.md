# Accordium

## Overview

**Accordium** is a **multi-party escrow system** built on the Stacks blockchain to ensure secure, transparent, and automated transactions between buyers, sellers, and arbiters. It introduces a robust dispute resolution process, dynamic fee handling, and detailed on-chain audit trails, making it ideal for decentralized marketplaces and trustless digital agreements.

## Key Features

* **Secure Escrow Creation**: Buyers, sellers, and arbiters are registered for each transaction, ensuring clear participant roles.
* **Automated Fund Management**: STX deposits, transfers, and payouts are handled automatically through on-chain logic.
* **Dispute Resolution**: Arbiters can resolve disputes with percentage-based settlements for fair fund distribution.
* **Reputation Tracking**: Arbiters have performance and reputation metrics to maintain accountability.
* **Comprehensive Logging**: Every escrow action (creation, funding, dispute, settlement) is tracked via detailed history logs.
* **Emergency Controls**: System owner can pause operations or trigger emergency mode to protect funds during anomalies.
* **Dynamic Fee Model**: Supports customizable protocol and arbiter fees with precision-based calculations.

## Contract Components

### Data Structures

* **`escrow-registry`**: Stores details of each escrow (participants, status, amount, and expiry).
* **`arbiter-registry`**: Manages arbiter profiles, fee rates, and reputation scores.
* **`deposit-log`**: Records all STX deposits with timestamps and block heights.
* **`history-log`**: Maintains a full audit trail of escrow events for transparency.
* **`participant-registry`**: Maps each escrow’s participants with their roles.
* **`sequence-tracker`**: Ensures sequential logging of escrow actions.

### Core Functions

* **`new-escrow`**: Creates an escrow contract with buyer, seller, and arbiter details.
* **`join-arbiters` / `update-profile`**: Registers and updates arbiters with fee configurations.
* **`deposit-funds`**: Allows participants to fund the escrow securely.
* **`mark-complete`**: Confirms completion by buyer or seller and releases payment automatically.
* **`raise-dispute` / `settle-dispute`**: Enables disputes and arbiter settlements with weighted payouts.
* **`process-refund`**: Allows refunds if escrows expire or remain unfunded.
* **`trigger-emergency` / `set-paused`**: Grants administrative control for contract safety.

### Read-Only Queries

* **`fetch-escrow`**, **`fetch-arbiter`**, **`fetch-history`**, **`fetch-stats`**: Retrieve live contract data for analytics or user dashboards.

## Summary

Accordium establishes a **trustless, auditable, and fair transaction environment** for digital marketplaces. It integrates dispute management, arbiter reputation, and system-level safety mechanisms to minimize risks and enhance user confidence in decentralized escrow operations.
