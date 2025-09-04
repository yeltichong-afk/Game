;; brief-rfp-winning.clar
;; Unique short Clarity contract for fair RFP (Google Clarity Web3)

(clarity-version 2)

;; Errors
(define-constant ERR-NOT-FOUND (err u100))
(define-constant ERR-BAD-TIME (err u101))
(define-constant ERR-HASH-MISMATCH (err u102))

;; Data
(define-data-var next-id uint u0)
(define-map rfps ((id uint))
  ((owner principal) (commit-end uint) (reveal-end uint) (winner (optional principal))))

(define-map commits ((id uint) (vendor principal)) ((h (buff 32))))
(define-map reveals ((id uint) (vendor principal)) ((uri (string-utf8 80))))

;; Create an RFP
(define-public (create-rfp (commit-end uint) (reveal-end uint))
  (begin
    (asserts! (> commit-end block-height) ERR-BAD-TIME)
    (asserts! (> reveal-end commit-end) ERR-BAD-TIME)
    (let ((id (+ (var-get next-id) u1)))
      (map-set rfps { id: id } { owner: tx-sender, commit-end: commit-end, reveal-end: reveal-end, winner: none })
      (var-set next-id id)
      (ok id))))

;; Vendor commit: sha256(id || vendor || uri || salt)
(define-public (commit (id uint) (h (buff 32)))
  (let ((rfp (map-get? rfps { id: id })))
    (match rfp
      some (asserts! (<= block-height (get commit-end (unwrap! rfp ERR-NOT-FOUND))) ERR-BAD-TIME)
      (map-set commits { id: id, vendor: tx-sender } { h: h })
      (ok true))
      none ERR-NOT-FOUND)))

;; Vendor reveal
(define-public (reveal (id uint) (uri (string-utf8 80)) (salt (buff 32)))
  (let ((c (map-get? commits { id: id, vendor: tx-sender })))
    (match c
      somec (let ((calc (sha256 (concat (to-buff id)
                                        (concat (to-buff (hash160 tx-sender))
                                        (concat (utf8-to-bytes uri) salt))))))
                  (stored (get h (unwrap! c ERR-NOT-FOUND))))
              (asserts! (is-eq stored calc) ERR-HASH-MISMATCH)
              (map-set reveals { id: id, vendor: tx-sender } { uri: uri })
              (ok true))
      none ERR-NOT-FOUND)))

;; Finalize winner (by owner)
(define-public (finalize (id uint) (winner principal))
  (let ((rfp (map-get? rfps { id: id })))
    (match rfp
      some (let ((data (unwrap! rfp ERR-NOT-FOUND)))
             (asserts! (is-eq (get owner data) tx-sender) ERR-NOT-FOUND)
             (asserts! (> block-height (get reveal-end data)) ERR-BAD-TIME)
             (map-set rfps { id: id } (merge data { winner: (some winner) }))
             (ok winner))
      none ERR-NOT-FOUND)))
