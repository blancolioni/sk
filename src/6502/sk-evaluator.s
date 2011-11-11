
;;         function Eval_App (Item   : Object)
;;                      return Object

        ;; Item -> R0
        ;; Return value -> R0

sk__evaluator__eval_app:
;;       It          : Object := Item;
        ;; It -> R0
;;       Changed     : Boolean := True;
        ;;  Changed -> R1
        lda     #1
        sta     r1
;;    begin

;;         while Changed loop

L1:     lda     r1
        bne     L2
        jmp     L3

L2:
;;          Changed := False;
        lda     #0
        sta     r1

;;          while Is_Application (It) loop

L4:     lda     r0
        and     #3
        cmp     #2
        beq     L5
        jmp     L6
L5:

;;             SK.Stack.Push (It);
        lda     R0
        sta     Alloc_Car
        lda     R0 + 1
        sta     Alloc_Car + 1
        lda

