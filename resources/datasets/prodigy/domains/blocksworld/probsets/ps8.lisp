;  11 probs max 5 obs, 5 goals
(setq *TEST-PROBS*
    '((BB8-1 (and (clear A) (on-table A) (arm-empty))
            ((object B) (clear B) (on-table B) (object A) (holding A)))
     (BB8-2 (and (on B A) (clear C))
           ((object D)
            (on-table D)
            (object C)
            (on C D)
            (object A)
            (on A C)
            (clear A)
            (object B)
            (clear B)
            (on-table B)
            (arm-empty)))
     (BB8-3 (and (clear D) (on-table B) (on-table D))
            ((object E)
             (on-table E)
             (object D)
             (on D E)
             (object C)
             (on C D)
             (object B)
             (on B C)
             (clear B)
             (object A)
             (holding A)))
     (BB8-4 (and (holding D) (on-table A) (on B A) (clear C) (clear B))
            ((object D)
             (on-table D)
             (object C)
             (on C D)
             (object B)
             (on B C)
             (object A)
             (on A B)
             (clear A)
             (arm-empty)))
     (BB8-5 (and (on C A) (clear C) (on-table A))
            ((object C)
             (on-table C)
             (object B)
             (on B C)
             (object A)
             (on A B)
             (clear A)
             (arm-empty)))
; known conceptual bug
;     (BB8-6 (and (arm-empty) (clear A) (on A C))
;            ((object D)
;             (on-table D)
;             (object B)
;             (on B D)
;             (clear B)
;             (object C)
;             (clear C)
;             (on-table C)
;;             (object A)
;             (holding A)))
     (BB8-7 (and (on C B) (on B D) (arm-empty) (on A C))
            ((object D)
             (on-table D)
             (object C)
             (on C D)
             (object B)
             (on B C)
             (object A)
             (on A B)
             (clear A)
             (arm-empty)))
;     (BB8-8 (and (arm-empty) (clear B) (on C A) (on-table A) (on-table B))
;            ((object D)
;             (on-table D)
;             (object C)
;             (on C D)
;             (object B)
;             (on B C)
;             (clear B)
;             (object A)
;             (holding A)))
     (BB8-9 (and (on-table D) (on-table B) (holding A))
            ((object E)
             (on-table E)
             (object D)
             (on D E)
             (object C)
             (on C D)
             (object B)
             (on B C)
             (object A)
             (on A B)
             (clear A)
             (arm-empty)))
     (BB8-10 (and (on D A) (on-table B) (on-table C) (on A C))
            ((object D)
             (on-table D)
             (object C)
             (on C D)
             (object B)
             (on B C)
             (object A)
             (on A B)
             (clear A)
             (arm-empty)))
     (BB8-11 (and (on B C) (on A D) (on C A) (on-table D) (clear E))
            ((object C)
             (on-table C)
             (object B)
             (on B C)
             (object A)
             (on A B)
             (clear A)
             (object E)
             (on-table E)
             (object D)
             (on D E)
             (clear D)
             (arm-empty)))))

