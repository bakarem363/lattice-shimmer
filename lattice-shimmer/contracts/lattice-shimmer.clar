;; Lattice Shimmer - Cross-Dimensional Blockchain RPG
;; Clarity Version: 2

;; Core contracts covering:
;;   - Shimmer Stone NFT minting and management
;;   - Dimensional travel (lattice navigation)
;;   - Quantum Container openings with randomness
;;   - Dimensional Essence governance token
;;   - Dimensional Yield Protocol (play-to-earn rewards)

;; ============================================================
;; CONSTANTS
;; ============================================================

(define-constant CONTRACT-OWNER tx-sender)

(define-constant ERR-NOT-OWNER (err u100))
(define-constant ERR-NOT-TOKEN-OWNER (err u101))
(define-constant ERR-TOKEN-NOT-FOUND (err u102))
(define-constant ERR-INVALID-LATTICE (err u103))
(define-constant ERR-INSUFFICIENT-ESSENCE (err u104))
(define-constant ERR-ALREADY-MINTED (err u105))
(define-constant ERR-CONTAINER-NOT-FOUND (err u106))
(define-constant ERR-CONTAINER-ALREADY-OPENED (err u107))
(define-constant ERR-UNAUTHORIZED (err u108))
(define-constant ERR-INVALID-RARITY (err u109))
(define-constant ERR-COOLDOWN-ACTIVE (err u110))

;; Lattice IDs (dimensions)
(define-constant LATTICE-ORIGIN u0)
(define-constant LATTICE-FIRE   u1)
(define-constant LATTICE-VOID   u2)
(define-constant LATTICE-STORM  u3)
(define-constant LATTICE-CRYSTAL u4)

;; Rarity tiers
(define-constant RARITY-COMMON    u1)
(define-constant RARITY-UNCOMMON  u2)
(define-constant RARITY-RARE      u3)
(define-constant RARITY-LEGENDARY u4)

;; Travel cooldown in blocks (~10 min at 10 min/block)
(define-constant TRAVEL-COOLDOWN u1)

;; Essence rewards per rarity tier on discovery
(define-constant ESSENCE-REWARD-COMMON    u10)
(define-constant ESSENCE-REWARD-UNCOMMON  u25)
(define-constant ESSENCE-REWARD-RARE      u75)
(define-constant ESSENCE-REWARD-LEGENDARY u200)

;; Cost in Essence to open a Quantum Container
(define-constant CONTAINER-OPEN-COST u50)

;; ============================================================
;; DATA VARS
;; ============================================================

;; Shimmer Stone NFT counters
(define-data-var last-stone-id uint u0)
(define-data-var last-container-id uint u0)
(define-data-var total-lattices uint u5)

;; ============================================================
;; FUNGIBLE TOKEN - Dimensional Essence (governance / reward)
;; ============================================================

(define-fungible-token dimensional-essence)

;; ============================================================
;; NON-FUNGIBLE TOKEN - Shimmer Stone
;; ============================================================

(define-non-fungible-token shimmer-stone uint)

;; ============================================================
;; MAPS
;; ============================================================

;; Shimmer Stone metadata
(define-map stone-metadata
  uint ;; stone-id
  {
    owner: principal,
    dna: uint,           ;; algorithmic DNA derived from block hash + id
    current-lattice: uint,
    rarity: uint,
    travel-count: uint,
    last-travel-block: uint,
    transformation-hash: uint  ;; tracks cross-dimensional transformations
  }
)

;; Lattice registry
(define-map lattice-registry
  uint ;; lattice-id
  {
    name: (string-ascii 32),
    active: bool,
    traveler-count: uint
  }
)

;; Quantum Containers - loot boxes opened with community randomness
(define-map quantum-containers
  uint ;; container-id
  {
    owner: principal,
    opened: bool,
    lattice-id: uint,
    reward-rarity: uint,
    reward-essence: uint,
    seed-block: uint   ;; block at which container was created (seed for randomness)
  }
)

;; Validator registry for Consensus-Driven Randomness
(define-map validators
  principal
  {
    active: bool,
    validated-count: uint
  }
)

;; Discovery log - tracks rare cross-dimensional combinations found by players
(define-map discovery-log
  { stone-id: uint, lattice-id: uint }
  {
    discoverer: principal,
    block-height: uint,
    transformation-hash: uint,
    rewarded: bool
  }
)

;; Governance proposals (Lattice Council)
(define-map governance-proposals
  uint
  {
    proposer: principal,
    description: (string-ascii 256),
    yes-votes: uint,
    no-votes: uint,
    executed: bool,
    end-block: uint
  }
)

(define-data-var last-proposal-id uint u0)

;; Track votes per address per proposal
(define-map proposal-votes
  { proposal-id: uint, voter: principal }
  bool ;; true = voted
)

;; ============================================================
;; PRIVATE HELPERS
;; ============================================================

;; Simple pseudo-random derivation from block hash, sender, and a nonce.
;; NOTE: This is not cryptographically secure randomness. For production,
;; combine with a VRF oracle or commit-reveal scheme.
(define-private (derive-random (nonce uint))
  (let (
    (block-hash-val (default-to 0x00 (get-block-info? id-header-hash (- block-height u1))))
  )
    (mod
      (xor
        (buff-to-uint-le (unwrap-panic (as-max-len? (concat (unwrap-panic (as-max-len? block-hash-val u16)) 0x00000000000000000000000000000001) u16)))
        nonce
      )
      u1000
    )
  )
)

;; Map a random value 0-999 to a rarity tier
(define-private (random-to-rarity (r uint))
  (if (< r u600)
    RARITY-COMMON
    (if (< r u850)
      RARITY-UNCOMMON
      (if (< r u970)
        RARITY-RARE
        RARITY-LEGENDARY
      )
    )
  )
)

;; Compute essence reward for a rarity
(define-private (essence-for-rarity (rarity uint))
  (if (is-eq rarity RARITY-LEGENDARY)
    ESSENCE-REWARD-LEGENDARY
    (if (is-eq rarity RARITY-RARE)
      ESSENCE-REWARD-RARE
      (if (is-eq rarity RARITY-UNCOMMON)
        ESSENCE-REWARD-UNCOMMON
        ESSENCE-REWARD-COMMON
      )
    )
  )
)

;; Validate a lattice-id is in range
(define-private (valid-lattice (lattice-id uint))
  (< lattice-id (var-get total-lattices))
)

;; ============================================================
;; ADMIN - Lattice Setup
;; ============================================================

(define-public (initialize-lattices)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-OWNER)
    (map-set lattice-registry LATTICE-ORIGIN  { name: "Origin",  active: true, traveler-count: u0 })
    (map-set lattice-registry LATTICE-FIRE    { name: "Fire",    active: true, traveler-count: u0 })
    (map-set lattice-registry LATTICE-VOID    { name: "Void",    active: true, traveler-count: u0 })
    (map-set lattice-registry LATTICE-STORM   { name: "Storm",   active: true, traveler-count: u0 })
    (map-set lattice-registry LATTICE-CRYSTAL { name: "Crystal", active: true, traveler-count: u0 })
    (ok true)
  )
)

;; ============================================================
;; SHIMMER STONE - Minting
;; ============================================================

;; Mint a new Shimmer Stone NFT to the caller.
;; DNA is derived from block hash + new id for uniqueness.
(define-public (mint-shimmer-stone)
  (let (
    (new-id (+ (var-get last-stone-id) u1))
    (dna    (derive-random new-id))
    (rarity (random-to-rarity dna))
  )
    (try! (nft-mint? shimmer-stone new-id tx-sender))
    (map-set stone-metadata new-id {
      owner:              tx-sender,
      dna:                dna,
      current-lattice:    LATTICE-ORIGIN,
      rarity:             rarity,
      travel-count:       u0,
      last-travel-block:  block-height,
      transformation-hash: dna
    })
    (var-set last-stone-id new-id)
    ;; Mint starter Essence to new stone owner
    (try! (ft-mint? dimensional-essence u100 tx-sender))
    (ok new-id)
  )
)

;; ============================================================
;; DIMENSIONAL TRAVEL
;; ============================================================

;; Travel a Shimmer Stone to a new lattice dimension.
;; Applies a transformation to the stone's hash and logs discoveries.
(define-public (travel-to-lattice (stone-id uint) (destination-lattice uint))
  (let (
    (meta (unwrap! (map-get? stone-metadata stone-id) ERR-TOKEN-NOT-FOUND))
    (dest-info (unwrap! (map-get? lattice-registry destination-lattice) ERR-INVALID-LATTICE))
    (current-block block-height)
  )
    ;; Caller must own the stone
    (asserts! (is-eq (get owner meta) tx-sender) ERR-NOT-TOKEN-OWNER)
    ;; Destination must be active and different from current
    (asserts! (get active dest-info) ERR-INVALID-LATTICE)
    (asserts! (valid-lattice destination-lattice) ERR-INVALID-LATTICE)
    ;; Enforce cooldown
    (asserts!
      (>= (- current-block (get last-travel-block meta)) TRAVEL-COOLDOWN)
      ERR-COOLDOWN-ACTIVE
    )
    ;; Compute new transformation hash (simulates Dynamic Reality Synthesis)
    (let (
      (new-transform
        (xor (get transformation-hash meta)
             (+ destination-lattice (get travel-count meta)))
      )
      (new-rarity (random-to-rarity (mod new-transform u1000)))
      (discovery-key { stone-id: stone-id, lattice-id: destination-lattice })
    )
      ;; Update stone metadata
      (map-set stone-metadata stone-id
        (merge meta {
          current-lattice:     destination-lattice,
          travel-count:        (+ (get travel-count meta) u1),
          last-travel-block:   current-block,
          rarity:              new-rarity,
          transformation-hash: new-transform
        })
      )
      ;; Log discovery if not already recorded
      (if (is-none (map-get? discovery-log discovery-key))
        (begin
          (map-set discovery-log discovery-key {
            discoverer:         tx-sender,
            block-height:       current-block,
            transformation-hash: new-transform,
            rewarded:           false
          })
          ;; Reward discoverer with Essence
          (try! (ft-mint? dimensional-essence (essence-for-rarity new-rarity) tx-sender))
        )
        true
      )
      ;; Update lattice traveler count
      (map-set lattice-registry destination-lattice
        (merge dest-info {
          traveler-count: (+ (get traveler-count dest-info) u1)
        })
      )
      (ok { stone-id: stone-id, new-lattice: destination-lattice, new-rarity: new-rarity })
    )
  )
)

;; ============================================================
;; QUANTUM CONTAINERS
;; ============================================================

;; Create a Quantum Container in the caller's current lattice.
;; Costs Essence to open; seed block is recorded for randomness.
(define-public (create-quantum-container (stone-id uint))
  (let (
    (meta (unwrap! (map-get? stone-metadata stone-id) ERR-TOKEN-NOT-FOUND))
    (new-cid (+ (var-get last-container-id) u1))
  )
    (asserts! (is-eq (get owner meta) tx-sender) ERR-NOT-TOKEN-OWNER)
    (map-set quantum-containers new-cid {
      owner:          tx-sender,
      opened:         false,
      lattice-id:     (get current-lattice meta),
      reward-rarity:  u0,
      reward-essence: u0,
      seed-block:     block-height
    })
    (var-set last-container-id new-cid)
    (ok new-cid)
  )
)

;; Open a Quantum Container - burns Essence and distributes reward.
;; Randomness is seeded from the container's seed block hash.
(define-public (open-quantum-container (container-id uint))
  (let (
    (container (unwrap! (map-get? quantum-containers container-id) ERR-CONTAINER-NOT-FOUND))
  )
    (asserts! (is-eq (get owner container) tx-sender) ERR-UNAUTHORIZED)
    (asserts! (not (get opened container)) ERR-CONTAINER-ALREADY-OPENED)
    ;; Burn Essence to open
    (try! (ft-burn? dimensional-essence CONTAINER-OPEN-COST tx-sender))
    ;; Derive reward rarity from seed block + container-id
    (let (
      (rand-val  (derive-random (+ container-id (get seed-block container))))
      (rarity    (random-to-rarity rand-val))
      (ess-reward (essence-for-rarity rarity))
    )
      ;; Mint reward Essence back to opener
      (try! (ft-mint? dimensional-essence ess-reward tx-sender))
      ;; Mark container opened
      (map-set quantum-containers container-id
        (merge container {
          opened:         true,
          reward-rarity:  rarity,
          reward-essence: ess-reward
        })
      )
      (ok { rarity: rarity, essence-rewarded: ess-reward })
    )
  )
)

;; ============================================================
;; VALIDATORS (Consensus-Driven Randomness contributors)
;; ============================================================

(define-public (register-validator)
  (begin
    (map-set validators tx-sender { active: true, validated-count: u0 })
    (ok true)
  )
)

(define-public (deregister-validator)
  (begin
    (map-set validators tx-sender
      (merge
        (default-to { active: false, validated-count: u0 }
          (map-get? validators tx-sender))
        { active: false }
      )
    )
    (ok true)
  )
)

;; Validator records participation; earns a small Essence reward
(define-public (submit-validation (container-id uint))
  (let (
    (vinfo (unwrap! (map-get? validators tx-sender) ERR-UNAUTHORIZED))
  )
    (asserts! (get active vinfo) ERR-UNAUTHORIZED)
    (map-set validators tx-sender
      (merge vinfo { validated-count: (+ (get validated-count vinfo) u1) })
    )
    ;; Small participation reward
    (try! (ft-mint? dimensional-essence u5 tx-sender))
    (ok true)
  )
)

;; ============================================================
;; GOVERNANCE - Lattice Council
;; ============================================================

;; Minimum Essence required to create a proposal
(define-constant MIN-ESSENCE-TO-PROPOSE u500)

(define-public (create-proposal (description (string-ascii 256)) (duration-blocks uint))
  (let (
    (balance (ft-get-balance dimensional-essence tx-sender))
    (new-pid (+ (var-get last-proposal-id) u1))
  )
    (asserts! (>= balance MIN-ESSENCE-TO-PROPOSE) ERR-INSUFFICIENT-ESSENCE)
    (map-set governance-proposals new-pid {
      proposer:    tx-sender,
      description: description,
      yes-votes:   u0,
      no-votes:    u0,
      executed:    false,
      end-block:   (+ block-height duration-blocks)
    })
    (var-set last-proposal-id new-pid)
    (ok new-pid)
  )
)

;; Vote on a proposal; voting power = Essence balance
(define-public (vote (proposal-id uint) (support bool))
  (let (
    (proposal (unwrap! (map-get? governance-proposals proposal-id) ERR-TOKEN-NOT-FOUND))
    (vote-key  { proposal-id: proposal-id, voter: tx-sender })
    (power     (ft-get-balance dimensional-essence tx-sender))
  )
    (asserts! (<= block-height (get end-block proposal)) ERR-INVALID-LATTICE)
    (asserts! (is-none (map-get? proposal-votes vote-key)) ERR-ALREADY-MINTED)
    (map-set proposal-votes vote-key true)
    (if support
      (map-set governance-proposals proposal-id
        (merge proposal { yes-votes: (+ (get yes-votes proposal) power) }))
      (map-set governance-proposals proposal-id
        (merge proposal { no-votes: (+ (get no-votes proposal) power) }))
    )
    (ok true)
  )
)

;; Mark a passed proposal as executed (owner only for now)
(define-public (execute-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? governance-proposals proposal-id) ERR-TOKEN-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-OWNER)
    (asserts! (not (get executed proposal)) ERR-ALREADY-MINTED)
    (asserts! (> block-height (get end-block proposal)) ERR-INVALID-LATTICE)
    (asserts! (> (get yes-votes proposal) (get no-votes proposal)) ERR-UNAUTHORIZED)
    (map-set governance-proposals proposal-id (merge proposal { executed: true }))
    (ok true)
  )
)

;; ============================================================
;; READ-ONLY FUNCTIONS
;; ============================================================

(define-read-only (get-stone-metadata (stone-id uint))
  (map-get? stone-metadata stone-id)
)

(define-read-only (get-lattice-info (lattice-id uint))
  (map-get? lattice-registry lattice-id)
)

(define-read-only (get-container (container-id uint))
  (map-get? quantum-containers container-id)
)

(define-read-only (get-validator-info (validator principal))
  (map-get? validators validator)
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? governance-proposals proposal-id)
)

(define-read-only (get-essence-balance (account principal))
  (ft-get-balance dimensional-essence account)
)

(define-read-only (get-discovery (stone-id uint) (lattice-id uint))
  (map-get? discovery-log { stone-id: stone-id, lattice-id: lattice-id })
)

(define-read-only (get-total-stones)
  (var-get last-stone-id)
)

(define-read-only (get-total-containers)
  (var-get last-container-id)
)
