;; Asset Registration Contract
;; Manages registration and basic information of public infrastructure assets

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_ASSET_EXISTS (err u101))
(define-constant ERR_ASSET_NOT_FOUND (err u102))
(define-constant ERR_INVALID_INPUT (err u103))

;; Asset data structure
(define-map assets
  { asset-id: uint }
  {
    name: (string-ascii 50),
    asset-type: (string-ascii 20),
    location: (string-ascii 100),
    installation-date: uint,
    estimated-value: uint,
    owner: principal,
    is-active: bool
  }
)

;; Asset counter
(define-data-var asset-counter uint u0)

;; Authorized managers
(define-map authorized-managers principal bool)

;; Register a new infrastructure asset
(define-public (register-asset (name (string-ascii 50))
                              (asset-type (string-ascii 20))
                              (location (string-ascii 100))
                              (installation-date uint)
                              (estimated-value uint))
  (let ((new-id (+ (var-get asset-counter) u1)))
    (asserts! (is-authorized tx-sender) ERR_UNAUTHORIZED)
    (asserts! (> (len name) u0) ERR_INVALID_INPUT)
    (asserts! (> estimated-value u0) ERR_INVALID_INPUT)

    (map-set assets
      { asset-id: new-id }
      {
        name: name,
        asset-type: asset-type,
        location: location,
        installation-date: installation-date,
        estimated-value: estimated-value,
        owner: tx-sender,
        is-active: true
      })

    (var-set asset-counter new-id)
    (ok new-id)))

;; Get asset information
(define-read-only (get-asset (asset-id uint))
  (map-get? assets { asset-id: asset-id }))

;; Update asset status
(define-public (update-asset-status (asset-id uint) (is-active bool))
  (let ((asset (unwrap! (get-asset asset-id) ERR_ASSET_NOT_FOUND)))
    (asserts! (is-authorized tx-sender) ERR_UNAUTHORIZED)

    (map-set assets
      { asset-id: asset-id }
      (merge asset { is-active: is-active }))
    (ok true)))

;; Authorization functions
(define-public (add-manager (manager principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set authorized-managers manager true)
    (ok true)))

(define-read-only (is-authorized (caller principal))
  (or (is-eq caller CONTRACT_OWNER)
      (default-to false (map-get? authorized-managers caller))))

;; Get total assets count
(define-read-only (get-total-assets)
  (var-get asset-counter))
