;; Title: EDE005 Dev Fund
;; Author: Marvin Janssen
;; Depends-On: EDP000
;; Synopsis:
;; A simple pre-seeded dev fund that can pay out developers on a monthly basis.
;; Description:
;; Initialised by EDP001 Dev Fund. Developers can be awarded a monthly allowance
;; and can claim it from this extension. Principals can be added and removed, and
;; allowances can be changed via future proposals.

(define-constant one-month-time u4380) ;; 43,800 minutes / 10 minute average block time.

(define-constant err-unauthorised (err u3000))
(define-constant err-no-allowance (err u3001))
(define-constant err-already-claimed (err u3002))
(define-constant err-reentry (err u3003))        ;; added: reentrancy error
(define-constant err-invalid-start (err u3004))  ;; added: invalid start-height

(define-map monthly-developer-allowances principal {start-height: uint, allowance: uint})
(define-map claim-counts principal uint)

;; added: reentrancy guard for claim operations
(define-data-var claim-lock bool false)

;; --- Authorisation check
(define-public (is-dao-or-extension)
	(begin
		;; safely evaluate external is-extension call and default to false
		(let ((is-ext (default-to false (contract-call? .executor-dao is-extension contract-caller))))
			(ok (asserts! (or (is-eq tx-sender .executor-dao) is-ext) err-unauthorised))
		)
	)
)

;; --- Internal DAO functions

(define-public (set-developer-allowance (start-height uint) (allowance uint) (who principal))
	(begin
		(try! (is-dao-or-extension))
		(ok (map-set monthly-developer-allowances who {start-height: start-height, allowance: allowance}))
	)
)

(define-private (set-developer-allowances-iter (item {start-height: uint, allowance: uint, who: principal}) (previous bool))
	(map-set monthly-developer-allowances (get who item) {start-height: (get start-height item), allowance: (get allowance item)})
)

(define-public (set-developer-allowances (developers (list 200 {start-height: uint, allowance: uint, who: principal})))
	(begin
		(try! (is-dao-or-extension))
		(ok (fold set-developer-allowances-iter developers true))
	)
)

(define-public (transfer (amount uint) (recipient principal) (memo (optional (buff 34))))
	(begin
		(try! (is-dao-or-extension))
		;; propagate failures from the token contract
		(try! (as-contract (contract-call? .ede000-governance-token transfer amount tx-sender recipient memo)))
	)
)

;; --- Public functions

(define-read-only (get-developer-allowance (who principal))
	(map-get? monthly-developer-allowances who)
)

(define-read-only (get-developer-claim-count (who principal))
	(default-to u0 (map-get? claim-counts who))
)

;; added: simple enter/exit lock helpers for claim()
(define-private (enter-claim-lock)
  (begin
    (asserts! (is-eq (var-get claim-lock) false) err-reentry)
    (var-set claim-lock true)
    (ok true)
  )
)

(define-private (exit-claim-lock)
  (ok (var-set claim-lock false))
)

(define-public (claim (memo (optional (buff 34))))
	(begin
		(try! (enter-claim-lock))
		(let
			(
				(entry (unwrap! (get-developer-allowance tx-sender) err-no-allowance))
				(claim-count (get-developer-claim-count tx-sender))
				(start-height (get start-height entry))
			)
			;; ensure the schedule has started and allowance is positive
			(try! (asserts! (>= block-height start-height) err-invalid-start))
			(try! (asserts! (> (get allowance entry) u0) err-no-allowance))
			(let
				(
					(max-claims (/ (- block-height start-height) one-month-time))
					(due (- max-claims claim-count))
					(developer tx-sender)
				)
				(asserts! (> due u0) err-already-claimed)
				(map-set claim-counts tx-sender max-claims)
				;; ensure token transfer succeeds; propagate errors
				(try! (as-contract (contract-call? .ede000-governance-token transfer (* due (get allowance entry)) tx-sender developer memo)))
			)
		)
		(try! (exit-claim-lock))
	)
)

;; --- Extension callback

(define-public (callback (sender principal) (memo (buff 34)))
	(ok true)
)
