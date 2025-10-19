;; Media Reef Atelier - Living Canvas NFT Platform
;; A decentralized creative economy platform with evolving NFTs

;; Define NFT trait
(define-non-fungible-token living-canvas uint)

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-token-exists (err u102))
(define-constant err-token-not-found (err u103))
(define-constant err-invalid-parent (err u104))
(define-constant err-listing-not-found (err u105))
(define-constant err-insufficient-payment (err u106))

;; Data variables
(define-data-var last-token-id uint u0)
(define-data-var platform-fee-percent uint u5) ;; 5% platform fee

;; Data maps
(define-map token-metadata
    uint
    {
        creator: principal,
        parent-id: (optional uint),
        generation: uint,
        uri: (string-ascii 256),
        created-at: uint
    }
)

(define-map token-derivatives
    uint
    (list 100 uint)
)

(define-map token-engagement
    uint
    {
        interactions: uint,
        derivative-count: uint
    }
)

(define-map marketplace-listings
    uint
    {
        seller: principal,
        price: uint,
        active: bool
    }
)

;; Read-only functions
(define-read-only (get-last-token-id)
    (ok (var-get last-token-id))
)

(define-read-only (get-token-owner (token-id uint))
    (ok (nft-get-owner? living-canvas token-id))
)

(define-read-only (get-token-metadata (token-id uint))
    (ok (map-get? token-metadata token-id))
)

(define-read-only (get-token-derivatives (token-id uint))
    (ok (default-to (list) (map-get? token-derivatives token-id)))
)

(define-read-only (get-token-engagement (token-id uint))
    (ok (map-get? token-engagement token-id))
)

(define-read-only (get-marketplace-listing (token-id uint))
    (ok (map-get? marketplace-listings token-id))
)

(define-read-only (get-platform-fee)
    (ok (var-get platform-fee-percent))
)

;; Private functions
(define-private (is-token-owner (token-id uint) (sender principal))
    (is-eq sender (unwrap! (nft-get-owner? living-canvas token-id) false))
)

;; Public functions

;; Create a seed NFT (genesis token)
(define-public (create-seed-nft (uri (string-ascii 256)))
    (let
        (
            (token-id (+ (var-get last-token-id) u1))
        )
        (try! (nft-mint? living-canvas token-id tx-sender))
        (map-set token-metadata token-id {
            creator: tx-sender,
            parent-id: none,
            generation: u0,
            uri: uri,
            created-at: block-height
        })
        (map-set token-engagement token-id {
            interactions: u0,
            derivative-count: u0
        })
        (var-set last-token-id token-id)
        (ok token-id)
    )
)

;; Create a derivative NFT (branching from parent)
(define-public (create-derivative-nft (parent-id uint) (uri (string-ascii 256)))
    (let
        (
            (token-id (+ (var-get last-token-id) u1))
            (parent-metadata (unwrap! (map-get? token-metadata parent-id) err-token-not-found))
            (parent-derivatives (default-to (list) (map-get? token-derivatives parent-id)))
            (parent-engagement (unwrap! (map-get? token-engagement parent-id) err-token-not-found))
        )
        ;; Mint new derivative NFT
        (try! (nft-mint? living-canvas token-id tx-sender))
        
        ;; Set metadata for derivative
        (map-set token-metadata token-id {
            creator: tx-sender,
            parent-id: (some parent-id),
            generation: (+ (get generation parent-metadata) u1),
            uri: uri,
            created-at: block-height
        })
        
        ;; Initialize engagement
        (map-set token-engagement token-id {
            interactions: u0,
            derivative-count: u0
        })
        
        ;; Update parent's derivatives list
        (map-set token-derivatives parent-id (unwrap-panic (as-max-len? (append parent-derivatives token-id) u100)))
        
        ;; Update parent engagement
        (map-set token-engagement parent-id {
            interactions: (+ (get interactions parent-engagement) u1),
            derivative-count: (+ (get derivative-count parent-engagement) u1)
        })
        
        (var-set last-token-id token-id)
        (ok token-id)
    )
)

;; Transfer NFT
(define-public (transfer (token-id uint) (sender principal) (recipient principal))
    (begin
        (asserts! (is-token-owner token-id sender) err-not-token-owner)
        (nft-transfer? living-canvas token-id sender recipient)
    )
)

;; Record interaction with token
(define-public (record-interaction (token-id uint))
    (let
        (
            (engagement (unwrap! (map-get? token-engagement token-id) err-token-not-found))
        )
        (map-set token-engagement token-id {
            interactions: (+ (get interactions engagement) u1),
            derivative-count: (get derivative-count engagement)
        })
        (ok true)
    )
)

;; List token for sale
(define-public (list-for-sale (token-id uint) (price uint))
    (begin
        (asserts! (is-token-owner token-id tx-sender) err-not-token-owner)
        (map-set marketplace-listings token-id {
            seller: tx-sender,
            price: price,
            active: true
        })
        (ok true)
    )
)

;; Cancel listing
(define-public (cancel-listing (token-id uint))
    (let
        (
            (listing (unwrap! (map-get? marketplace-listings token-id) err-listing-not-found))
        )
        (asserts! (is-eq (get seller listing) tx-sender) err-not-token-owner)
        (map-set marketplace-listings token-id (merge listing { active: false }))
        (ok true)
    )
)

;; Purchase listed token
(define-public (purchase-token (token-id uint))
    (let
        (
            (listing (unwrap! (map-get? marketplace-listings token-id) err-listing-not-found))
            (price (get price listing))
            (seller (get seller listing))
            (platform-fee (/ (* price (var-get platform-fee-percent)) u100))
            (seller-amount (- price platform-fee))
        )
        (asserts! (get active listing) err-listing-not-found)
        
        ;; Transfer payment to seller
        (try! (stx-transfer? seller-amount tx-sender seller))
        
        ;; Transfer platform fee to contract owner
        (try! (stx-transfer? platform-fee tx-sender contract-owner))
        
        ;; Transfer NFT to buyer
        (try! (nft-transfer? living-canvas token-id seller tx-sender))
        
        ;; Deactivate listing
        (map-set marketplace-listings token-id (merge listing { active: false }))
        
        (ok true)
    )
)

;; Update platform fee (owner only)
(define-public (set-platform-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set platform-fee-percent new-fee)
        (ok true)
    )
)