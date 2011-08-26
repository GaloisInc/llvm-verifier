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
