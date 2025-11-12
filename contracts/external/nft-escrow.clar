;; An example external contract to show how the ExecutorDAO is able to
;; manage external contracts that might not be aware of the DAO. See
;; edp003-whitelist-escrow-nft for more details.

(define-constant err-not-contract-owner (err u100))
(define-constant err-not-whitelisted (err u101))
(define-constant err-unknown-escrow (err u102))
(define-constant err-wrong-nft (err u103))
(define-constant err-not-nft-owner (err u104))
(define-constant err-reentry (err u105))               ;; added: reentrancy error
(define-constant err-invalid-price (err u106))         ;; added: sanity error

(define-data-var contract-owner principal tx-sender)
(define-map nft-whitelist principal bool)
(define-map nfts-in-escrow {token-id: uint, recipient: principal} {owner: principal, price: uint, asset: principal})

;; added: simple reentrancy protection
(define-data-var reentry-lock bool false)

(define-trait sip009-transferable
	(
		(transfer (uint principal principal) (response bool uint))
	)
)

(define-private (is-owner)
	(ok (asserts! (is-eq (var-get contract-owner) tx-sender) err-not-contract-owner))
)

(define-read-only (get-contract-owner)
	(ok (var-get contract-owner))
)

(define-public (set-contract-owner (new-owner principal))
	(begin
		(try! (is-owner))
		(ok (var-set contract-owner new-owner))
	)
)

(define-read-only (is-whitelisted (nft principal))
	(default-to false (map-get? nft-whitelist nft))
)

(define-public (set-whitelisted (nft principal) (enabled bool))
	(begin
		(try! (is-owner))
		(ok (map-set nft-whitelist nft enabled))
	)
)

;; added: enter/exit lock helpers
(define-private (enter-lock)
  (begin
    (asserts! (is-eq (var-get reentry-lock) false) err-reentry)
    (var-set reentry-lock true)
    (ok true)
  )
)

(define-private (exit-lock)
  (ok (var-set reentry-lock false))
)

(define-read-only (get-escrow (token-id uint) (recipient principal))
	(map-get? nfts-in-escrow {token-id: token-id, recipient: recipient})
)

;; changed: make send-nft return the external response (caller will try! it)
(define-private (send-nft (token-id uint) (recipient principal) (nft <sip009-transferable>))
	(contract-call? nft transfer token-id (as-contract) recipient)
)

(define-public (place-in-escrow (token-id uint) (recipient principal) (amount uint) (nft <sip009-transferable>))
	(begin
		(try! (enter-lock))                                     ;; added: protect external calls
		(asserts! (is-whitelisted (contract-of nft)) err-not-whitelisted)
		(map-set nfts-in-escrow {token-id: token-id, recipient: recipient} {owner: tx-sender, price: amount, asset: (contract-of nft)})
		(try! (contract-call? nft transfer token-id tx-sender (as-contract))) ;; changed: propagate failures
		(try! (exit-lock))                                      ;; added: release lock
	)
)

(define-public (pay-and-redeem (token-id uint) (nft <sip009-transferable>))
	(let ((escrow (unwrap! (get-escrow token-id tx-sender) err-unknown-escrow)))
		(try! (enter-lock))                                     ;; added: protect external calls
		(asserts! (is-eq (contract-of nft) (get asset escrow)) err-wrong-nft)
		(asserts! (>= (get price escrow) u0) err-invalid-price)  ;; added: basic sanity check
		(map-delete nfts-in-escrow {token-id: token-id, recipient: tx-sender})
		(try! (stx-transfer? (get price escrow) tx-sender (get owner escrow)))
		(try! (send-nft token-id tx-sender nft))                ;; changed: require successful send
		(try! (exit-lock))                                      ;; added: release lock
	)
)

(define-public (cancel-escrow (token-id uint) (recipient principal) (nft <sip009-transferable>))
	(let ((escrow (unwrap! (get-escrow token-id recipient) err-unknown-escrow)))
		(try! (enter-lock))                                     ;; added: protect external calls
		(asserts! (is-eq (get owner escrow) tx-sender) err-not-nft-owner)
		(asserts! (is-eq (contract-of nft) (get asset escrow)) err-wrong-nft)
		(map-delete nfts-in-escrow {token-id: token-id, recipient: recipient})
		(try! (send-nft token-id tx-sender nft))                ;; changed: require successful send
		(try! (exit-lock))                                      ;; added: release lock
	)
)

