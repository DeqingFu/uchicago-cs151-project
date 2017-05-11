#lang typed/racket
(require "../uchicago151/uchicago151.rkt")
(require "../uchicago151/uc151image.rkt")
(require "../uchicago151/uc151universe.rkt")
(require "../project2/option.rkt")
(require "../project2/loc.rkt")
(require "chess-logic.rkt")

;;==== READ ME ====
;;This is the GUI of Chess
;;Click on a square to select
;;Click on the selected again to un-select
;;The user's legal moves are highlighted in blue
;;Click on blue square to move your selected piece
;;If the user's pawn is available to promote
;;Press cartain keys to promote
;;Press q to Quenn, r to Rook, b to Bishop, n to Knight.
;;Choose not to promote by pressing Shift and choose another destination
;;At any time of the game, the user can press Escape key to quit the game
;;Press "g" to reset the game
;;White moves first
;;If the user wants to castle when available, move the king to its destination,
;;and the system will do the castling automatically
;;At any time, press Shift to take one move back(regret).

(define-struct Dims
  ([width  : Integer]
   [height : Integer]))

(define-struct (Sized T)
  ([t    : T]
   [dims : Dims]))

(define-type (Reporter T)
  (T -> String))

;;asked Hanqian Zhang about struct of ChessWorld
(define-struct ChessWorld
  ([game : (Sized ChessGame)]
   [moveFrom : (Option Loc)]
   [moveTo : (Option Loc)]
   [promoStop? : Boolean]
   [info : (Sized (Reporter ChessWorld))]
   [quit? : Boolean]))

(: guarantee-byte (Integer -> Byte))
;;guanrantee that a integer is a byte
(define (guarantee-byte n)
  (if (byte? n) n (error "not a byte")))

(: repo (Reporter ChessWorld))
;;reporter of the chess
(define (repo world)
  (match world
    [(ChessWorld (Sized game dimB) LocF LocT promoS repo quit?)
     (match game
       [(ChessGame board p _ _)
        (cond
          [(checkmate? game)
           (string-append "Checkmate!"
                          "  Winner:  "
                          (symbol->string (opponent p))
                          ".")]
          [(stalemate? game) "Stalemate"]
          [promoS (string-append
                   "Press Key to Promote"
                   "q: Queen, r: Rook, b: Bishop, n: Knight, Shift: SKIP")]
          [else (if (in-check? game)
                    (string-append
                     (symbol->string p)
                     "'s turn and you are in check.")
                    (string-append
                     (symbol->string p) "'s turn."))])])]))

(: new-chess-world : ChessGame Dims Dims -> ChessWorld)
;;intial world
(define (new-chess-world game dimB dimR)
  (ChessWorld (Sized game dimB) 'None 'None #f (Sized repo dimR) #f))

(: handle-click (ChessWorld Integer Integer Mouse-Event -> ChessWorld))
;;handle a click on the world
(define (handle-click world x y mouse)
  (match mouse
    ["button-down"
     (match world
       [(ChessWorld (Sized game dimB) LocF LocT promoS repo~ quit?)
        (if promoS world
            (match* (dimB game)
              [((Dims w h) (ChessGame board p _ _))
               (local
                 {(define clickLOC
                    (Loc
                     (quotient y (quotient h 8))
                     (quotient x (quotient w 8))))}
                 (if (> y h) world
                     ;;prevent accidental click on the reporter to report error
                     (match* (LocF LocT)
                       [('None 'None)
                        (ChessWorld (Sized game dimB)
                                    (Some clickLOC) 'None #f repo~ quit?)]
                       [((Some loc) 'None)
                        (match
                            (filter (λ ([m : Move])
                                      (match m
                                        [(StdMove src dst _ _)
                                         (equal? clickLOC dst)]
                                        [(CastleMove king-src king-dst _ _ _)
                                         (equal? clickLOC king-dst)]
                                        [(PromoMove src dst _ _ _)
                                         (equal? clickLOC dst)]))
                                    (filter
                                     (λ ([m~ : Move]) (legal-move? game m~))
                                     (available-moves-piece game loc)))
                          ['() (ChessWorld
                                (Sized game dimB) 'None 'None #f repo~ quit?)]
                          [someMove
                           (local
                             {(: contain-promo? : (Listof Move) -> Boolean)
                              (define (contain-promo? ms)
                                (match
                                    (filter (lambda ([m : Move])
                                              (match m
                                                [(PromoMove _ _ _ _ _) #t]
                                                [_ #f])) ms)
                                  ['() #f]
                                  [_ #t]))}
                             (cond
                               [(contain-promo? someMove)
                                (ChessWorld
                                 (Sized game dimB) LocF
                                 (Some clickLOC) #t repo~ quit?)]
                               [else
                                (ChessWorld
                                 (Sized (apply-move game (first someMove)) dimB)
                                 'None 'None promoS repo~ quit?)]))])]
                       [((Some _) (Some _)) world])))]))])]
    [_ world]))

(: draw-chess-board : ChessGame (Option Loc) (Option Loc) Dims -> Image)
;; draw the board
(define (draw-chess-board game from to dim)
  (match* (dim game)
    [((Dims n _) (ChessGame board _ _ _))
     (local
       {(define side (quotient n 8))
        (define available-locs
          (if (none? from) '()
              (map (λ ([m : Move])
                     (match m
                       [(CastleMove _ king-dst _ _ _)
                        king-dst]
                       [(PromoMove _ dst _ _ _)
                        dst]
                       [(StdMove _ dst _ _)
                        dst]))
                   (filter
                    (λ ([m~ : Move]) (legal-move? game m~))
                    (available-moves-piece game (val-of from))))))
        (: in-locs? : Integer Integer (Listof Loc) -> Boolean)
        (define (in-locs? r c locs)
          (ormap (λ ([loc : Loc]) (equal? (Loc r c) loc)) locs))
        (: board->image : Board -> Image)
        ;; draw an image of a certain board
        (define (board->image board)
          (local
            {(: loop : Board Integer -> Image)
             (define (loop bd r)
               (if (> r 7) empty-image
                   (above
                    (local
                      {(: row (Integer -> Image))
                       (define (row c)
                         (if (> c 7) empty-image
                             (beside
                              (match (board-ref bd (Loc r c))
                                ['None
                                 (if (in-locs? r c available-locs)
                                     (if (= (remainder (+ r c) 2) 0)
                                         (square side "solid"
                                                 (make-color 204 255 255))
                                         (square side "solid"
                                                 (make-color 51 153 255)))
                                     (if (and (not (none? from))
                                              (= (Loc-row (val-of from)) r)
                                              (= (Loc-col (val-of from)) c))
                                         (square side "solid" "white")    
                                         (if (= (remainder (+ r c) 2) 0)
                                             (square side "solid" "beige")
                                             (square side "solid" "brown"))))]
                                [(Some (Piece type color))
                                 (overlay
                                  (scale
                                   0.618
                                   (match* (type color)
                                     [('King 'Black)
                                      (text "♚" (guarantee-byte side) "black")]
                                     [('King 'White)
                                      (text "♔" (guarantee-byte side) "black")]
                                     [('Queen 'Black)
                                      (text "♛" (guarantee-byte side) "black")]
                                     [('Queen 'White)
                                      (text "♕"  (guarantee-byte side) "black")]
                                     [('Rook 'Black)
                                      (text "♜" (guarantee-byte side) "black")]
                                     [('Rook 'White)
                                      (text "♖" (guarantee-byte side) "black")]
                                     [('Bishop 'Black)
                                      (text "♝" (guarantee-byte side) "black")]
                                     [('Bishop 'White)
                                      (text "♗" (guarantee-byte side) "black")]
                                     [('Knight 'Black)
                                      (text "♞" (guarantee-byte side) "black")]
                                     [('Knight 'White)
                                      (text "♘" (guarantee-byte side) "black")]
                                     [('Pawn 'Black)
                                      (text "♟" (guarantee-byte side) "black")]
                                     [('Pawn 'White)
                                      (text "♙" (guarantee-byte side)
                                            "black")]))
                                  (if (in-locs? r c available-locs)
                                      (if (= (remainder (+ r c) 2) 0)
                                          (square side "solid"
                                                  (make-color 204 255 255))
                                          (square side "solid"
                                                  (make-color 51 153 255)))
                                      (if (and (not (none? from))
                                               (= (Loc-row (val-of from)) r)
                                               (= (Loc-col (val-of from)) c))
                                          (square side "solid" "white")
                                          (if
                                           (= (remainder (+ r c) 2) 0)
                                           (square side "solid" "beige")
                                           (square side "solid" "brown")))))])
                              (row (add1 c)))))}
                      (row 0))
                    (loop bd (add1 r)))))}
            (loop board 0)))
        }
       (board->image board))]))

(: handle-key : ChessWorld String -> ChessWorld)
;;use key to promote
;;use key to quit and regret (take one move back)
;;use key to reset game
(define (handle-key w k)
  (match w
    [(ChessWorld (Sized g dimB) from to promoS repo quit?)
     (match repo
       [(Sized _ dimR)
        (cond
          [promoS
           (match g
             [(ChessGame b p his cas)
              (local
                {(define proQ (PromoMove (val-of from) (val-of to)
                                         (val-of (board-ref b (val-of from)))
                                         (board-ref b (val-of to)) 'Queen))
                 (define proR (PromoMove (val-of from) (val-of to)
                                         (val-of (board-ref b (val-of from)))
                                         (board-ref b (val-of to)) 'Rook))
                 (define proB (PromoMove (val-of from) (val-of to)
                                         (val-of (board-ref b (val-of from)))
                                         (board-ref b (val-of to)) 'Bishop))
                 (define proN (PromoMove (val-of from) (val-of to)
                                         (val-of (board-ref b (val-of from)))
                                         (board-ref b (val-of to)) 'Knight))}
                (match k
                  [(or "Q" "q")
                   (ChessWorld
                    (Sized (apply-move g proQ) dimB)
                    'None 'None #f repo quit?)]
                  [(or "R" "r")
                   (ChessWorld
                    (Sized (apply-move g proR) dimB)
                    'None 'None #f repo quit?)]
                  [(or "B" "b")
                   (ChessWorld
                    (Sized (apply-move g proB) dimB)
                    'None 'None #f repo quit?)]
                  [(or "N" "n")
                   (ChessWorld
                    (Sized (apply-move g proN) dimB)
                    'None 'None #f repo quit?)]
                  ["shift" (ChessWorld (Sized g dimB) from 'None #f repo quit?)]
                  ["g" (new-chess-world new-game dimB dimR)]
                  ["escape" (ChessWorld (Sized g dimB) from to promoS repo #t)]
                  [_ w]))])]
          [else (match k
                  ["g" (new-chess-world new-game dimB dimR)]
                  ["escape" (ChessWorld (Sized g dimB) from to promoS repo #t)]
                  ["shift"
                   (match g
                     [(ChessGame _ _ '() _) w]
                     [_ (ChessWorld (Sized (take-back-move g) dimB)
                                    'None 'None promoS repo #f)])]
                  [_ w])])])]))

(: esc? : ChessWorld -> Boolean)
;; quit or not
(define (esc? world)
  (ChessWorld-quit? world))

(: draw-chess-world : ChessWorld -> Image)
;; draw the chess board and the reporter together
(define (draw-chess-world world)
  (above
   (match world
     [(ChessWorld (Sized game dimB) from to _ _ _)
      (draw-chess-board game from to dimB)])
   (local
     {(: draw-report : ChessWorld -> Image)
      (define (draw-report w)
        (match w
          [(ChessWorld _ _ _ promoS (Sized _ dimRepo) _)
           (match dimRepo
             [(Dims wid hei)
              (overlay
               (above
                (if promoS
                    (above
                     (text/font "Press Key to Promote" 20 "black" #f
                                'roman 'normal 'bold #f)
                     (text/font "Q ⇒ Queen, N ⇒ Knight" 15 "black" #f
                                'roman 'normal 'normal #f)
                     (text/font "R ⇒ Rook,  B ⇒ Bishop" 15 "black" #f
                                'roman 'normal 'normal #f))
                    (text/font (repo w) 20 "black" #f
                               'roman 'normal 'bold #f))
                (text/font  "Press Esc to quit. Press Shift to take back one move."
                            20 "black" #f
                            'roman 'normal 'normal #f))
                       (rectangle wid hei "solid" "gray"))])]))}
     (draw-report world))))

(: main (ChessGame Dims Dims -> ChessWorld))
(define (main g dimBoard dimRepo)
  (big-bang
   (new-chess-world new-game dimBoard dimRepo) : ChessWorld
   [to-draw draw-chess-world]
   [on-mouse handle-click]
   [on-key handle-key]
   [stop-when esc?]
   [name "Chess"]))

(main new-game (Dims 480 480) (Dims 480 100)) 
