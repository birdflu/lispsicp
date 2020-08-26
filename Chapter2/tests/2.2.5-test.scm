#lang scheme

(include "../exercise2.2.5.scm")

(paint wave)
(paint (flip-vert wave ))
(paint (shrink-to-upper-right wave ))
(paint (rotate90 wave ))
(paint (squash-inwards wave ))
(paint (beside wave wave))