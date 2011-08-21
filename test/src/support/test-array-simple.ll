define i32 @main() nounwind ssp {
  %arr = alloca [1 x i32], align 4
  %1 = getelementptr inbounds [1 x i32]* %arr, i32 0, i64 0
  store i32 42, i32* %1
  %2 = getelementptr inbounds [1 x i32]* %arr, i32 0, i64 0
  %3 = load i32* %2
  ret i32 %3
}
