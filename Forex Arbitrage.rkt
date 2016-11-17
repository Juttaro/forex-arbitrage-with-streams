;Streams applied to Foreign Exchange Arbitrage
;Connie Liu & James Uttaro
;CSc 335: Programming Language Paradigms
;Professor Douglas Troeger

#lang racket
(#%require graph) ;https://stchang.github.io/graph/graph.html


;stream auxillary functions 
(define-syntax delay
  (syntax-rules ()
    ((_ exp) (lambda () exp))))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (force delayed-object) (delayed-object))

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define the-empty-stream '())

(define stream-null? null?)



;hard coded graphs from real triangular forex arbitrage
;
; this is an example of curgraph which is the negative-log applied to the (stream-car graph-stream)
;(define curgraph (directed-graph '((usd usd) (usd eur) (usd gbp)
;                           (eur usd) (eur eur) (eur gbp)
;                           (gbp usd) (gbp eur) (gbp gbp)
;                           )
;                         '(1 0.2997546536860502 0.4200712604975265
;                            -0.3093635772256188 1 0.12783337150988489
;                            -0.4193680132771557 -0.11867152971749854 1)))



(define A (directed-graph '((usd usd) (usd eur) (usd gbp)
                           (eur usd) (eur eur) (eur gbp)
                           (gbp usd) (gbp eur) (gbp gbp)
                           )
                         '(1 .741 .657
                            1.349 1 .888
                            1.521  1.126 1)))

(define B (directed-graph '((usd usd) (usd eur) (usd gbp)
                           (eur usd) (eur eur) (eur gbp)
                           (gbp usd) (gbp eur) (gbp gbp)
                           )
                         '(1 .741 .657
                            1.449 1 .888
                            1.521  1.126 1)))

(define C (directed-graph '((usd usd) (usd eur) (usd gbp)
                           (eur usd) (eur eur) (eur gbp)
                           (gbp usd) (gbp eur) (gbp gbp)
                           )
                         '(1 .741 .627
                            1.349 1 .888
                            1.521  1.326 1)))

(define (weight-list graph)
 (let ((x (get-edges graph)))
   (define (iter lst)
     (cond ((null? lst) '())           
     (else (cons (edge-weight graph (caar lst) (cadar lst)) (iter (cdr lst))))))
 (iter x)))

(define (neg-log-proc input)
  (* -1 (log input)))

(define (neg-log-weights graph)
  (map neg-log-proc (weight-list graph)))


(define (Arbitrage-bellman-ford G s)
  (define w (if (weighted-graph? G) (λ (u v) (edge-weight G u v)) (λ _ 1)))

  ;; init
  (define-vertex-property G d #:init +inf.0) 
  (define-vertex-property G π #:init #f)
  (d-set! s 0)
  (define es (get-edges G))
  
  ;; compute result
  (for* ([_ (in-vertices G)] [e es])
    (match-define (list u v) e)
    ;; relax
    (when (> (d v) (+ (d u) (w u v)))
      (d-set! v (+ (d u) (w u v)))
      (π-set! v u)))
  
  
  (values (d->hash) (π->hash)))


;this functions checks if the graphs current and previous are equal to each other. If the currency rates have not changed then we return false,
;if the rates have changed then we return true. 
(define (check-graphs cur prev)
  (cond ((null? cur) '())
        ((null? prev) '())
        ((equal? cur prev) #t)
        (else #f)))



; We are using streams to handle real time currency exchange rates different intervals of time throughout the day
; instead of evaluating a batch data set all at once. It could be 10 second intervals or in one minute intervals, either way
; we will have large data which we represent as graphs coming in. 
; For simplicity reasons we hard coded graphs/matrices based upon real time foreign exchange market rates.
; Instead of creating a program that would take data from online and important it into our functoon
; Eventually streams would allow us to have infinite data sets without time complexity issues and other complications
; Graph-stream is the stream we have manually coded based on real foreign currency exchange rates 
; (best case scenario we will query data from an online exchange rates server but for now this is to show our objective)
; we have three graphs that we would like to find a profitable arbitrage if any.

(define graph-stream1 (cons-stream A (cons-stream B (cons-stream C null)))) ;goes to bellmanford 
(define graph-stream2 (cons-stream B (cons-stream A (cons-stream C null)))) ;no arbitrage eventually returns empty list
(define graph-stream3 (cons-stream A (cons-stream A (cons-stream A null))))

;stream-arb takes the stream, previous graph or original and the type of currency we are starting with
(define (stream-arb stream-dw-graph prev-graph source)
  (let (( cur-graph (directed-graph
                     (get-edges (stream-car graph-stream1))
                     (neg-log-weights (stream-car graph-stream1)))))
    (cond ((stream-null? stream-dw-graph) the-empty-stream)
        (else (cond ((eq? (check-graphs (car stream-dw-graph) prev-graph) #t)
                     (stream-arb (stream-cdr stream-dw-graph) (stream-car stream-dw-graph) source)) ;no arbitrage catch 
                    ((eq? (check-graphs (stream-car stream-dw-graph) prev-graph) #f)
                     (Arbitrage-bellman-ford cur-graph source))
                    (else ((stream-arb (stream-cdr stream-dw-graph) (stream-car stream-dw-graph) source))))))))
                            
(stream-arb graph-stream1 C 'eur); bellman-ford: negative weight cycle
;(stream-arb graph-stream2 A 'usd) ; '() no arbitrage found
;(stream-arb graph-stream3 A 'eur); bellman-ford: negative weight cycle




