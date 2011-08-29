@matrix_mul_4x4.a = internal constant [4 x [4 x i32]] [[4 x i32] [i32 0, i32 1, i32 2, i32 3], [4 x i32] [i32 4, i32 5, i32 6, i32 7], [4 x i32] [i32 8, i32 9, i32 10, i32 11], [4 x i32] [i32 12, i32 13, i32 14, i32 15]], align 16
@matrix_mul_4x4.b = internal constant [4 x [4 x i32]] [[4 x i32] [i32 15, i32 14, i32 13, i32 12], [4 x i32] [i32 11, i32 10, i32 9, i32 8], [4 x i32] [i32 7, i32 6, i32 5, i32 4], [4 x i32] [i32 3, i32 2, i32 1, i32 0]], align 16

declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture, i8* nocapture, i64, i32, i1) nounwind

define i32 @matrix_mul_4x4() nounwind ssp {
  %a = alloca [4 x [4 x i32]], align 16
  %b = alloca [4 x [4 x i32]], align 16
  %c = alloca [4 x [4 x i32]], align 16
  %i = alloca i32, align 4
  %j = alloca i32, align 4
  %k = alloca i32, align 4
  %1 = bitcast [4 x [4 x i32]]* %a to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %1, i8* bitcast ([4 x [4 x i32]]* @matrix_mul_4x4.a to i8*), i64 64, i32 16, i1 false)
  %2 = bitcast [4 x [4 x i32]]* %b to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %2, i8* bitcast ([4 x [4 x i32]]* @matrix_mul_4x4.b to i8*), i64 64, i32 16, i1 false)
  store i32 0, i32* %i, align 4
  br label %3

; <label>:3                                       ; preds = %52, %0
  %4 = load i32* %i, align 4
  %5 = icmp slt i32 %4, 4
  br i1 %5, label %6, label %55

; <label>:6                                       ; preds = %3
  store i32 0, i32* %j, align 4
  br label %7

; <label>:7                                       ; preds = %48, %6
  %8 = load i32* %j, align 4
  %9 = icmp slt i32 %8, 4
  br i1 %9, label %10, label %51

; <label>:10                                      ; preds = %7
  %11 = load i32* %j, align 4
  %12 = sext i32 %11 to i64
  %13 = load i32* %i, align 4
  %14 = sext i32 %13 to i64
  %15 = getelementptr inbounds [4 x [4 x i32]]* %c, i32 0, i64 %14
  %16 = getelementptr inbounds [4 x i32]* %15, i32 0, i64 %12
  store i32 0, i32* %16
  store i32 0, i32* %k, align 4
  br label %17

; <label>:17                                      ; preds = %44, %10
  %18 = load i32* %k, align 4
  %19 = icmp slt i32 %18, 4
  br i1 %19, label %20, label %47

; <label>:20                                      ; preds = %17
  %21 = load i32* %k, align 4
  %22 = sext i32 %21 to i64
  %23 = load i32* %i, align 4
  %24 = sext i32 %23 to i64
  %25 = getelementptr inbounds [4 x [4 x i32]]* %a, i32 0, i64 %24
  %26 = getelementptr inbounds [4 x i32]* %25, i32 0, i64 %22
  %27 = load i32* %26
  %28 = load i32* %j, align 4
  %29 = sext i32 %28 to i64
  %30 = load i32* %k, align 4
  %31 = sext i32 %30 to i64
  %32 = getelementptr inbounds [4 x [4 x i32]]* %b, i32 0, i64 %31
  %33 = getelementptr inbounds [4 x i32]* %32, i32 0, i64 %29
  %34 = load i32* %33
  %35 = mul nsw i32 %27, %34
  %36 = load i32* %j, align 4
  %37 = sext i32 %36 to i64
  %38 = load i32* %i, align 4
  %39 = sext i32 %38 to i64
  %40 = getelementptr inbounds [4 x [4 x i32]]* %c, i32 0, i64 %39
  %41 = getelementptr inbounds [4 x i32]* %40, i32 0, i64 %37
  %42 = load i32* %41
  %43 = add nsw i32 %42, %35
  store i32 %43, i32* %41
  br label %44

; <label>:44                                      ; preds = %20
  %45 = load i32* %k, align 4
  %46 = add nsw i32 %45, 1
  store i32 %46, i32* %k, align 4
  br label %17

; <label>:47                                      ; preds = %17
  br label %48

; <label>:48                                      ; preds = %47
  %49 = load i32* %j, align 4
  %50 = add nsw i32 %49, 1
  store i32 %50, i32* %j, align 4
  br label %7

; <label>:51                                      ; preds = %7
  br label %52

; <label>:52                                      ; preds = %51
  %53 = load i32* %i, align 4
  %54 = add nsw i32 %53, 1
  store i32 %54, i32* %i, align 4
  br label %3

; <label>:55                                      ; preds = %3
  %56 = getelementptr inbounds [4 x [4 x i32]]* %c, i32 0, i64 3
  %57 = getelementptr inbounds [4 x i32]* %56, i32 0, i64 3
  %58 = load i32* %57
  ret i32 %58
}
