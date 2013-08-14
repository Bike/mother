(defpackage #:mother
  (:use #:cl #:alexandria)
  (:shadow #:eval))

(in-package #:mother)

(defstruct %ignore)
(defvar *ignore* (make-%ignore))

(defmethod print-object ((object %ignore) stream) (write "#ignore" :stream stream :escape nil))

(defstruct %inert)
(defvar *inert* (make-%inert))

(defmethod print-object ((object %inert) stream) (write "#inert" :stream stream :escape nil))

(defstruct truth)
(defvar *true* (make-truth))
(defmethod print-object ((object truth) stream) (write "#t" :stream stream :escape nil))

(defstruct falsity)
(defvar *false* (make-falsity))
(defmethod print-object ((object falsity) stream) (write "#f" :stream stream :escape nil))

(deftype motherly-boolean () '(or truth falsity))

(declaim (inline kbool))
(defun kbool (b) (if b *true* *false*))

(deftype ptree () '(or null symbol %ignore cons))
