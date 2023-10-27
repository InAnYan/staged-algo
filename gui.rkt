#lang racket

(require racket/gui
         framework)

(require "stages.rkt")

(define keymap (keymap:get-editor))

(define frame (new frame% [label "Stages explorer"]
                   [width 1000]
                   [height 700]))

(define panel (new vertical-panel% [parent frame]))

(define input-text (new text%))
(send input-text set-keymap keymap)

(define input-editor (new editor-canvas% [parent panel]
                          [editor input-text]))

(define (eval-input-text btn e)
  (clear-current-stages)
  (send output-text lock #f)
  (send output-text erase)
  (send output-text insert
        (format "~s"
                (eval
                 (read
                  (open-input-string (send input-text get-text))))))
  (send output-text lock #t)
  (make-stages-gui stages-panel (reverse-stages current-stages)))

(define (reverse-stages stages)
  (map reverse-stage (reverse stages)))

(define (reverse-stage stage)
  (if (empty? stage)
      empty
      (if (second stage)
          (list (first stage) #t
                (map reverse-stage (reverse (third stage))))
          stage)))

(send keymap add-function "eval" eval-input-text)
(send keymap map-function "c:return" "eval")

(define eval-panel (new horizontal-panel% [parent panel]
                        [alignment '(center center)]
                        [stretchable-height #f]))

(define evaluate-btn (new button% [parent eval-panel]
                          [label "Evaluate"]
                          [callback eval-input-text]))

(define eval-msg (new message% [parent eval-panel]
                      [label "Or press Ctrl+Enter."]))

(define output-text (new text%))
(send output-text set-keymap keymap)
(send output-text insert "Hello!")
(send output-text lock #t)

(define output-editor (new editor-canvas% [parent panel]
                           [editor output-text]))

(define none-tabs '("None"))

(define stages-panel (new panel% [parent panel]))

(define (make-stages-gui parent stages)
  (send parent change-children (lambda (x) empty))

  (define (tabs-callback tabs e)
    (let* ([index (send tabs get-selection)]
           [stage (list-ref stages index)]
           [fn (first stage)]
           [data (third stage)])
      (if (second stage)
          (make-stages-gui tabs data)
          (make-data tabs data))))

  (define tabs (new tab-panel% [parent parent]
                    [choices (map (compose symbol->string first) stages)]
                    [callback tabs-callback]))

  (unless (empty? stages)
    (tabs-callback tabs 'tab-panel)))

(define (make-data parent data)
  (send parent change-children (lambda (x) empty))
  (define text (new text%))
  (send text insert (format "~v" data))
  (send text set-keymap keymap)
  (new editor-canvas% [parent parent]
       [editor text]))

(define (make-stages parent stages)
  empty)

(define (make-staged-gui parent stage)
  (new tab-panel% [parent parent]
       [choices (map (compose symbol->string first) stage)]))

(define (make-tree-gui parent stage)
  (make-staged-gui parent (second stage)))

(require racket/trace)
(trace make-stages-gui)
(trace make-tree-gui)
(trace make-staged-gui)

(provide frame)
