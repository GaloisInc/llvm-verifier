@onedim_init.a = internal constant [4 x i32] [i32 0, i32 1, i32 2, i32 3], align 16
@twodim.a = internal constant [4 x [4 x i32]] [[4 x i32] [i32 0, i32 1, i32 2, i32 3], [4 x i32] [i32 4, i32 5, i32 6, i32 7], [4 x i32] [i32 8, i32 9, i32 10, i32 11], [4 x i32] [i32 12, i32 13, i32 14, i32 15]], align 16

declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture, i8* nocapture, i64, i32, i1) nounwind

define i32 @arr1() nounwind ssp {
  %arr = alloca [1 x i32], align 4
  %1 = getelementptr inbounds [1 x i32]* %arr, i32 0, i64 0
  store i32 42, i32* %1
  %2 = getelementptr inbounds [1 x i32]* %arr, i32 0, i64 0
  %3 = load i32* %2
  ret i32 %3
}

define i32 @arr2() nounwind ssp {
  %arr = alloca [3 x i32], align 4
  %1 = getelementptr inbounds [3 x i32]* %arr, i32 0, i64 0
  store i32 42, i32* %1
  %2 = getelementptr inbounds [3 x i32]* %arr, i32 0, i64 1
  store i32 99, i32* %2
  %3 = getelementptr inbounds [3 x i32]* %arr, i32 0, i64 0
  %4 = load i32* %3
  %5 = getelementptr inbounds [3 x i32]* %arr, i32 0, i64 1
  %6 = load i32* %5
  %7 = add nsw i32 %4, %6
  %8 = getelementptr inbounds [3 x i32]* %arr, i32 0, i64 2
  store i32 %7, i32* %8
  %9 = getelementptr inbounds [3 x i32]* %arr, i32 0, i64 2
  %10 = load i32* %9
  ret i32 %10
}

define i32 @onedim_init() nounwind ssp {
  %a = alloca [4 x i32], align 16
  %1 = bitcast [4 x i32]* %a to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %1, i8* bitcast ([4 x i32]* @onedim_init.a to i8*), i64 16, i32 16, i1 false)
  %2 = getelementptr inbounds [4 x i32]* %a, i32 0, i64 3
  %3 = load i32* %2
  ret i32 %3
}

define i32 @twodim_init() nounwind ssp {
  %a = alloca [4 x [4 x i32]], align 16
  %1 = bitcast [4 x [4 x i32]]* %a to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %1, i8* bitcast ([4 x [4 x i32]]* @twodim.a to i8*), i64 64, i32 16, i1 false)
  %2 = getelementptr inbounds [4 x [4 x i32]]* %a, i32 0, i64 3
  %3 = getelementptr inbounds [4 x i32]* %2, i32 0, i64 3
  %4 = load i32* %3
  %5 = getelementptr inbounds [4 x [4 x i32]]* %a, i32 0, i64 1
  %6 = getelementptr inbounds [4 x i32]* %5, i32 0, i64 2
  %7 = load i32* %6
  %8 = add nsw i32 %4, %7
  ret i32 %8
}
