target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-darwin10.0.0"

define i32 @trivial_branch(i32 %b) nounwind ssp {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  store i32 %b, i32* %2, align 4
  %3 = load i32* %2, align 4
  %4 = icmp ne i32 %3, 0
  br i1 %4, label %5, label %6

; <label>:5                                       ; preds = %0
  store i32 1, i32* %1
  br label %7

; <label>:6                                       ; preds = %0
  store i32 0, i32* %1
  br label %7

; <label>:7                                       ; preds = %6, %5
  %8 = load i32* %1
  ret i32 %8
}

; int* sym_read_aux(int b, int* p, int* q)
; {
;     if (b)
;         return p;
;     else
;         return q;
; }
define i32* @sym_read_aux(i32 %b, i32* %p, i32* %q) nounwind ssp {
  %1 = alloca i32*, align 8
  %2 = alloca i32, align 4
  %3 = alloca i32*, align 8
  %4 = alloca i32*, align 8
  store i32 %b, i32* %2, align 4
  store i32* %p, i32** %3, align 8
  store i32* %q, i32** %4, align 8
  %5 = load i32* %2, align 4
  %6 = icmp ne i32 %5, 0
  br i1 %6, label %7, label %9

; <label>:7                                       ; preds = %0
  %8 = load i32** %3, align 8
  store i32* %8, i32** %1
  br label %11

; <label>:9                                       ; preds = %0
  %10 = load i32** %4, align 8
  store i32* %10, i32** %1
  br label %11

; <label>:11                                      ; preds = %9, %7
  %12 = load i32** %1
  ret i32* %12
}

; int sym_read(int b) 
; {
;     int x = 42, y = 99;
;     return *sym_read_aux(b, &x, &y);
; }
define i32 @sym_read(i32 %b) nounwind ssp {
  %1 = alloca i32, align 4
  %x = alloca i32, align 4
  %y = alloca i32, align 4
  store i32 %b, i32* %1, align 4
  store i32 42, i32* %x, align 4
  store i32 99, i32* %y, align 4
  %2 = load i32* %1, align 4
  %3 = call i32* @sym_read_aux(i32 %2, i32* %x, i32* %y)
  %4 = load i32* %3
  ret i32 %4
}
