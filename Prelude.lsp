(defmacro defun (name params body)
  (setq name (lambda params body)))

(defmacro or (a b)
  (if a a b))

(defmacro and (a b)
  (if a b nil))

(defun <= (x y)
  (or (< x y) (eq x y)))

(defun > (x y)
  (< y x))

(defun >= (x y)
  (<= y x))

(defun - (x y)
  (+ x (neg y)))

(defun nullp (x)
  (eq x nil))
