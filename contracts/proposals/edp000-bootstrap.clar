;; Title: EDP000 Bootstrap
;; Author: Marvin Janssen
;; Synopsis:
;; Boot proposal that sets the governance token, DAO parameters, and extensions, and
;; mints the initial governance tokens.
;; Description:
;; Mints the initial supply of governance tokens and enables the the following 
;; extensions: "EDE000 Governance Token", "EDE001 Proposal Voting",
;; "EDE002 Proposal Submission", "EDE003 Emergency Proposals",
;; "EDE004 Emergency Execute".

(impl-trait .proposal-trait.proposal-trait)

(define-constant err-invalid-caller (err u100))
(define-constant err-setup-failed (err u101))
(define-constant err-minting-failed (err u102))

(define-data-var dao-active bool false)
(define-data-var total-minted uint u0)

(define-public (is-dao-active)
  (ok (var-get dao-active))
)

(define-public (get-total-minted)
  (ok (var-get total-minted))
)

(define-private (validate-caller (sender principal))
  (is-eq sender tx-sender)
)

(define-public (execute (sender principal))
  (begin
    ;; Validate caller is the transaction sender
    (asserts! (validate-caller sender) err-invalid-caller)
    
    ;; Set DAO as active
    (var-set dao-active true)

    ;; Enable genesis extensions.
    (try! (contract-call? .executor-dao set-extensions
      (list
        {extension: .ede000-governance-token, enabled: true}
        {extension: .ede001-proposal-voting, enabled: true}
        {extension: .ede002-proposal-submission, enabled: true}
        {extension: .ede003-emergency-proposals, enabled: true}
        {extension: .ede004-emergency-execute, enabled: true}
        {extension: .ede005-treasury-management, enabled: false}  ;; New extension disabled by default
      )
    ))

    ;; Set emergency team members with validation
    (asserts! (contract-call? .ede003-emergency-proposals set-emergency-team-member 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM true) err-setup-failed)
    (asserts! (contract-call? .ede003-emergency-proposals set-emergency-team-member 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5 true) err-setup-failed)
    (try! (contract-call? .ede003-emergency-proposals set-emergency-team-member 'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG true))  ;; New emergency member
    (try! (contract-call? .ede003-emergency-proposals set-emergency-team-member 'ST2JHG361ZXG51QTKY2NQCVBPPRRE2KZB1HR05NNC true))  ;; New emergency member

    ;; Set executive team members with enhanced security
    (asserts! (contract-call? .ede004-emergency-execute set-executive-team-member 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM true) err-setup-failed)
    (asserts! (contract-call? .ede004-emergency-execute set-executive-team-member 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5 true) err-setup-failed)
    (asserts! (contract-call? .ede004-emergency-execute set-executive-team-member 'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG true) err-setup-failed)
    (asserts! (contract-call? .ede004-emergency-execute set-executive-team-member 'ST2JHG361ZXG51QTKY2NQCVBPPRRE2KZB1HR05NNC true) err-setup-failed)
    (try! (contract-call? .ede004-emergency-execute set-executive-team-member 'ST2NEB84ASENDXKYGJPQW86YXQCEFEX2ZQPG87ND true))  ;; New executive member
    (try! (contract-call? .ede004-emergency-execute set-signals-required u4))  ;; Increased from u3 to u4 for enhanced security

    ;; Track total minting amount
    (var-set total-minted u11000)

    ;; Mint initial token supply with validation
    (asserts! (contract-call? .ede000-governance-token edg-mint-many
      (list
        {amount: u1500, recipient: sender}  ;; Increased from u1000 to u1500
        {amount: u1500, recipient: 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5}  ;; Increased amount
        {amount: u1500, recipient: 'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG}  ;; Increased amount
        {amount: u1000, recipient: 'ST2JHG361ZXG51QTKY2NQCVBPPRRE2KZB1HR05NNC}
        {amount: u1000, recipient: 'ST2NEB84ASENDXKYGJPQW86YXQCEFEX2ZQPG87ND}
        {amount: u1000, recipient: 'ST2REHHS5J3CERCRBEPMGH7921Q6PYKAADT7JP2VB}
        {amount: u1000, recipient: 'ST3AM1A56AK2C1XAFJ4115ZSV26EB49BVQ10MGCS0}
        {amount: u1000, recipient: 'ST3NBRSFKX28FQ2ZJ1MAKX58HKHSDGNV5N7R21XCP}
        {amount: u1000, recipient: 'ST3PF13W7Z0RRM42A8VZRVFQ75SV1K26RXEP8YGKJ}
        {amount: u1000, recipient: 'STNHKEPYEPJ8ET55ZZ0M5A34J0R3N5FM2CMMMAZ6}
        {amount: u500, recipient: 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM}  ;; New recipient
      )
    ) err-minting-failed)

    ;; Emit setup completion event
    (print "ExecutorDAO has been successfully initialized and is now active.")
    (ok true)
  )
)

;; New helper function to check DAO status
(define-public (get-dao-status)
  (ok {
    active: (var-get dao-active),
    total-minted: (var-get total-minted),
    timestamp: block-height
  })
)