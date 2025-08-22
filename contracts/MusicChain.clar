;; MusicChain: Decentralized Music Streaming Platform
;; Version: 1.0.0
;; Stream and manage music collections with artist royalties on-chain

;; Platform Statistics
(define-data-var total-tracks uint u0)

;; Core Music Data
(define-map track-registry
  { track-id: uint }
  {
    title: (string-ascii 64),
    musician: principal,
    duration: uint,
    release-block: uint,
    genre: (string-ascii 128),
    instruments: (list 10 (string-ascii 32))
  })

(define-map streaming-permissions
  { track-id: uint, listener: principal }
  { can-stream: bool })

;; Platform Error Codes
(define-constant track-not-found (err u401))
(define-constant track-already-exists (err u402))
(define-constant invalid-title (err u403))
(define-constant invalid-duration (err u404))
(define-constant unauthorized-access (err u405))
(define-constant not-musician (err u406))
(define-constant admin-only (err u400))
(define-constant streaming-restricted (err u407))
(define-constant invalid-instruments (err u408))

;; Platform Administrator
(define-constant platform-admin tx-sender)

;; ===== Music Helper Functions =====

;; Check if track exists in platform
(define-private (track-exists (track-id uint))
  (is-some (map-get? track-registry { track-id: track-id })))

;; Verify musician ownership
(define-private (is-musician (track-id uint) (caller principal))
  (match (map-get? track-registry { track-id: track-id })
    track-data (is-eq (get musician track-data) caller)
    false
  ))

;; Get track duration
(define-private (get-track-duration (track-id uint))
  (default-to u0
    (get duration
      (map-get? track-registry { track-id: track-id })
    )
  ))

;; Validate instrument format
(define-private (is-valid-instrument (instrument (string-ascii 32)))
  (and
    (> (len instrument) u0)
    (< (len instrument) u33)
  ))

;; Validate all instruments in collection
(define-private (validate-instruments (instruments (list 10 (string-ascii 32))))
  (and
    (> (len instruments) u0)
    (<= (len instruments) u10)
    (is-eq (len (filter is-valid-instrument instruments)) (len instruments))
  ))

;; ===== Music Management Functions =====

;; Add new track to platform
(define-public (add-track
  (title (string-ascii 64))
  (duration uint)
  (genre (string-ascii 128))
  (instruments (list 10 (string-ascii 32))))
  (let
    (
      (next-id (+ (var-get total-tracks) u1))
    )
    ;; Validate inputs
    (asserts! (> (len title) u0) invalid-title)
    (asserts! (< (len title) u65) invalid-title)
    (asserts! (> duration u0) invalid-duration)
    (asserts! (< duration u3600) invalid-duration)
    (asserts! (> (len genre) u0) invalid-title)
    (asserts! (< (len genre) u129) invalid-title)
    (asserts! (validate-instruments instruments) invalid-instruments)
    
    ;; Register track
    (map-insert track-registry
      { track-id: next-id }
      {
        title: title,
        musician: tx-sender,
        duration: duration,
        release-block: stacks-block-height,
        genre: genre,
        instruments: instruments
      }
    )
    
    ;; Grant musician streaming permission
    (map-insert streaming-permissions
      { track-id: next-id, listener: tx-sender }
      { can-stream: true }
    )
    
    ;; Update counter
    (var-set total-tracks next-id)
    (ok next-id)
  ))

;; Update existing track details
(define-public (update-track
  (track-id uint)
  (new-title (string-ascii 64))
  (new-duration uint)
  (new-genre (string-ascii 128))
  (new-instruments (list 10 (string-ascii 32))))
  (let
    (
      (track-data (unwrap! (map-get? track-registry { track-id: track-id }) track-not-found))
    )
    ;; Verify permissions and inputs
    (asserts! (track-exists track-id) track-not-found)
    (asserts! (is-eq (get musician track-data) tx-sender) not-musician)
    (asserts! (> (len new-title) u0) invalid-title)
    (asserts! (< (len new-title) u65) invalid-title)
    (asserts! (> new-duration u0) invalid-duration)
    (asserts! (< new-duration u3600) invalid-duration)
    (asserts! (> (len new-genre) u0) invalid-title)
    (asserts! (< (len new-genre) u129) invalid-title)
    (asserts! (validate-instruments new-instruments) invalid-instruments)
    
    ;; Update track information
    (map-set track-registry
      { track-id: track-id }
      (merge track-data {
        title: new-title,
        duration: new-duration,
        genre: new-genre,
        instruments: new-instruments
      })
    )
    (ok true)
  ))

;; Remove track from platform
(define-public (remove-track (track-id uint))
  (let
    (
      (track-data (unwrap! (map-get? track-registry { track-id: track-id }) track-not-found))
    )
    ;; Verify musician ownership
    (asserts! (track-exists track-id) track-not-found)
    (asserts! (is-eq (get musician track-data) tx-sender) not-musician)
    
    ;; Remove from platform
    (map-delete track-registry { track-id: track-id })
    (ok true)
  ))

;; Transfer track to new musician
(define-public (transfer-track (track-id uint) (new-musician principal))
  (let
    (
      (track-data (unwrap! (map-get? track-registry { track-id: track-id }) track-not-found))
    )
    ;; Verify current musician
    (asserts! (track-exists track-id) track-not-found)
    (asserts! (is-eq (get musician track-data) tx-sender) not-musician)
    
    ;; Transfer ownership
    (map-set track-registry
      { track-id: track-id }
      (merge track-data { musician: new-musician })
    )
    (ok true)
  ))

;; ===== Read-Only Functions =====

;; Get total tracks in platform
(define-read-only (get-total-tracks)
  (var-get total-tracks))

;; Get track details
(define-read-only (get-track-info (track-id uint))
  (map-get? track-registry { track-id: track-id }))

;; Get streaming permission
(define-read-only (get-streaming-permission (track-id uint) (listener principal))
  (map-get? streaming-permissions { track-id: track-id, listener: listener }))

;; Get platform stats
(define-read-only (get-platform-stats)
  {
    admin: platform-admin,
    total-tracks: (var-get total-tracks)
  })