(setq *TEST-PROBS*
    '((CC0-1 (joined A B ORIENTATION-2)
            ((last-time 3)
             (is-bolt (B1 (1.2 cm)))
             (is-bolt (B2 (1.4 cm)))
             (last-scheduled B 0)
             (last-scheduled A 0)
             (has-hole B (1.4 cm) ORIENTATION-3)
             (temperature B COLD)
             (shape B CYLINDRICAL)
             (is-object B)
             (has-hole A (2 mm) ORIENTATION-1)
             (painted A (WATER-RES RED))
             (temperature A COLD)
             (shape A IRREGULAR)
             (is-object A)))
     (CC0-2 (has-hole A (6 mm) ORIENTATION-1)
            ((last-time 3)
             (is-bolt (B1 (1.2 cm)))
             (is-bolt (B2 (8 mm)))
             (last-scheduled B 0)
             (last-scheduled A 0)
             (has-hole B (2 mm) ORIENTATION-3)
             (surface-condition B SMOOTH)
             (temperature B COLD)
             (shape B RECTANGULAR)
             (is-object B)
             (has-hole A (1.2 cm) ORIENTATION-4)
             (temperature A COLD)
             (shape A IRREGULAR)
             (is-object A)))
     (CC0-3 (has-hole A (4 mm) ORIENTATION-2) 
            ((last-time 3)
             (is-bolt (B1 (1.4 cm)))
             (is-bolt (B2 (1.2 cm)))
             (last-scheduled B 0)
             (last-scheduled A 0)
             (temperature B COLD)
             (shape B RECTANGULAR)
             (is-object B)
             (has-hole A (1.2 cm) ORIENTATION-4)
             (painted A (WATER-RES WHITE))
             (surface-condition A POLISHED)
             (temperature A COLD)
             (shape A RECTANGULAR)
             (is-object A)))
     (CC0-4 (surface-condition A POLISHED)
            ((last-time 3)
             (is-bolt (B1 (4 mm)))
             (is-bolt (B2 (1.4 cm)))
             (last-scheduled B 0)
             (last-scheduled A 0)
             (has-hole B (8 mm) ORIENTATION-2)
             (painted B (REGULAR RED))
             (temperature B COLD)
             (shape B UNDETERMINED)
             (is-object B)
             (temperature A COLD)
             (shape A UNDETERMINED)
             (is-object A)))
     (CC0-5 (shape B CYLINDRICAL)
            ((last-time 3)
             (is-bolt (B1 (2 mm)))
             (is-bolt (B2 (4 mm)))
             (last-scheduled B 0)
             (last-scheduled A 0)
             (has-hole B (6 mm) ORIENTATION-2)
             (surface-condition B POLISHED)
             (temperature B COLD)
             (shape B UNDETERMINED)
             (is-object B)
             (surface-condition A POLISHED)
             (temperature A COLD)
             (shape A IRREGULAR)
             (is-object A)))
     (CC0-6 (joined A B ORIENTATION-3)
            ((last-time 3)
             (is-bolt (B1 (1 cm)))
             (is-bolt (B2 (6 mm)))
             (last-scheduled B 0)
             (last-scheduled A 0)
             (painted B (REGULAR RED))
             (surface-condition B SMOOTH)
             (temperature B COLD)
             (shape B IRREGULAR)
             (is-object B)
             (painted A (WATER-RES WHITE))
             (surface-condition A POLISHED)
             (temperature A COLD)
             (shape A RECTANGULAR)
             (is-object A)))
     (CC0-7 (surface-condition A SMOOTH)
            ((last-time 3)
             (is-bolt (B1 (2 mm)))
             (is-bolt (B2 (1.4 cm)))
             (last-scheduled B 0)
             (last-scheduled A 0)
             (temperature B COLD)
             (shape B IRREGULAR)
             (is-object B)
             (has-hole A (8 mm) ORIENTATION-2)
             (surface-condition A SMOOTH)
             (temperature A COLD)
             (shape A UNDETERMINED)
             (is-object A)))
     (CC0-8 (has-hole A (1.2 cm) ORIENTATION-3)
            ((last-time 3)
             (is-bolt (B1 (6 mm)))
             (is-bolt (B2 (2 mm)))
             (last-scheduled B 0)
             (last-scheduled A 0)
             (painted B (REGULAR WHITE))
             (temperature B COLD)
             (shape B RECTANGULAR)
             (is-object B)
             (temperature A COLD)
             (shape A RECTANGULAR)
             (is-object A)))
     (CC0-9 (painted A (WATER-RES WHITE))
            ((last-time 3)
             (is-bolt (B1 (8 mm)))
             (is-bolt (B2 (8 mm)))
             (last-scheduled B 0)
             (last-scheduled A 0)
             (painted B (WATER-RES WHITE))
             (surface-condition B POLISHED)
             (temperature B COLD)
             (shape B RECTANGULAR)
             (is-object B)
             (surface-condition A POLISHED)
             (temperature A COLD)
             (shape A UNDETERMINED)
             (is-object A)))
     (CC0-10 (painted B (WATER-RES RED))
            ((last-time 3)
             (is-bolt (B1 (4 mm)))
             (is-bolt (B2 (1.4 cm)))
             (last-scheduled B 0)
             (last-scheduled A 0)
             (painted B (WATER-RES WHITE))
             (temperature B COLD)
             (shape B CYLINDRICAL)
             (is-object B)
             (surface-condition A SMOOTH)
             (temperature A COLD)
             (shape A RECTANGULAR)
             (is-object A)))))


(setq *AUX-COMMANDS*
  '((CC0-1 (expand-all))
    (CC0-2 (expand-all))
    (CC0-3 (expand-all))
    (CC0-4 (expand-all))
    (CC0-5 (expand-all))
    (CC0-6 (expand-all))
    (CC0-7 (expand-all))
    (CC0-8 (expand-all))
    (CC0-9 (expand-all))
    (CC0-10 (expand-all))))













