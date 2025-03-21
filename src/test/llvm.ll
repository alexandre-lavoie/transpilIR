%opaque = type opaque
%struct = type { %opaque, [10 x i32] }
%union.0 = type { i32 }
%union.1 = type { float }
%union = type { %union.0 }
%t = type opaque

@dlink = thread_local global { i8 } { i8 0 }, section "section", partition "flags"
@dptr = hidden global { i32 } { i32 0 } 
@dvals = hidden global { [3 x i8], i8, i8, i8, [100 x i8], ptr } { [3 x i8] c"ABC", i8 32, i8 164, i8 61, [100 x i8] zeroinitializer, ptr getelementptr(i8, ptr @dptr, i64 1) }
@d = hidden global { i32 } { i32 0 }

declare void @fcall_0()
declare void @fcall_1()

define external void @flink() section "section" partition "flags" {
s.label:
    ret void
}

define private i32 @fparams(ptr %e, i32 %a, ptr %b) {
s.label:
    %r = add i32 %a, 0
    ret i32 %r
}

define private void @fmath() {
s.label:
    %r = sub i64 0, 0
    %r.1 = add i64 %r, 0
    %r.2 = and i64 %r.1, 0
    %r.3 = sdiv i64 %r.2, 0
    %r.4 = mul i64 %r.3, 0
    %r.5 = or i64 %r.4, 0
    %r.6 = srem i64 %r.5, 0
    %r.7 = ashr i64 %r.6, 0
    %r.8 = shl i64 %r.7, 0
    %r.9 = lshr i64 %r.8, 0
    %r.10 = sub i64 %r.9, 0
    %r.11 = udiv i64 %r.10, 0
    %r.12 = urem i64 %r.11, 0
    %r.13 = xor i64 %r.12, 0
    ret void
}

define private void @fmem() {
s.label:
    %l = alloca i64, i64 8, align 16
    %l.1 = alloca i64, i64 8, align 4
    %l.2 = alloca i64, i64 8, align 8
    call void @llvm.memcpy(ptr %l.2, ptr %l.2, i32 16, i1 false)
    %d = load double, ptr %l.2
    %l.3 = load i64, ptr %l.2
    %l.3.ptr = inttoptr i64 %l.3 to ptr
    %s = load float, ptr %l.3.ptr
    %b = load i8, ptr %l.3.ptr
    %h = load i16, ptr %l.3.ptr
    %w = load i32, ptr %l.3.ptr
    %b.1 = load i8, ptr %l.3.ptr
    %h.1 = load i16, ptr %l.3.ptr
    %w.1 = load i32, ptr %l.3.ptr
    %t = load ptr, ptr %l.3.ptr
    store i8 0, ptr %l.3.ptr
    store double 0.0, ptr %l.3.ptr
    store i16 0, ptr %l.3.ptr
    store i64 0, ptr %l.3.ptr
    store float 0.0, ptr %l.3.ptr
    store i32 0, ptr %l.3.ptr
    ret void
}

define private void @fcmp() {
s.label:
    %r = fcmp oeq double 0.0, 0.0
    %r.1 = icmp eq i64 0, 0
    %r.2 = fcmp oeq float 0.0, 0.0
    %r.3 = icmp eq i32 0, 0
    %r.4 = fcmp oge double 0.0, 0.0
    %r.5 = fcmp oge float 0.0, 0.0
    %r.6 = fcmp ogt double 0.0, 0.0
    %r.7 = fcmp ogt float 0.0, 0.0
    %r.8 = fcmp ole double 0.0, 0.0
    %r.9 = fcmp ole float 0.0, 0.0
    %r.10 = fcmp olt double 0.0, 0.0
    %r.11 = fcmp olt float 0.0, 0.0
    %r.12 = fcmp one double 0.0, 0.0
    %r.13 = icmp ne i64 0, 0
    %r.14 = fcmp one float 0.0, 0.0
    %r.15 = icmp ne i32 0, 0
    %r.16 = fcmp ord double 0.0, 0.0
    %r.17 = fcmp ord float 0.0, 0.0
    %r.18 = icmp sge i64 0, 0
    %r.19 = icmp sge i32 0, 0
    %r.20 = icmp sgt i64 0, 0
    %r.21 = icmp sgt i32 0, 0
    %r.22 = icmp sle i64 0, 0
    %r.23 = icmp sle i32 0, 0
    %r.24 = icmp slt i64 0, 0
    %r.25 = icmp slt i32 0, 0
    %r.26 = icmp uge i64 0, 0
    %r.27 = icmp uge i32 0, 0
    %r.28 = icmp ugt i64 0, 0
    %r.29 = icmp ugt i32 0, 0
    %r.30 = icmp ule i64 0, 0
    %r.31 = icmp ule i32 0, 0
    %r.32 = icmp ult i64 0, 0
    %r.33 = icmp ult i32 0, 0
    %r.34 = fcmp uno double 0.0, 0.0
    %r.35 = fcmp uno float 0.0, 0.0
    ret void
}

define private void @fconv() {
s.label:
    %l = fptosi double 0.0 to i64
    %l.1 = fptoui double 0.0 to i64
    %d = fpext float 0.0 to double
    %w = sext i8 0 to i32
    %w.2 = sext i16 0 to i32
    %l.2 = sext i32 0 to i64
    %w.3 = zext i8 0 to i32
    %w.4 = zext i16 0 to i32
    %l.3 = zext i32 0 to i64
    %d.1 = sitofp i64 0 to double
    %d.2 = uitofp i64 0 to double
    %w.5 = fptosi float 0.0 to i32
    %w.6 = fptoui float 0.0 to i32
    %s = sitofp i32 0 to float
    %s.1 = uitofp i32 0 to float
    %s.2 = fptrunc double 0.0 to float
    %w.7 = add i32 0, 0
    %w.8 = bitcast float 0.0 to i32
    %t = fptoui double 0.0 to i64
    ret void
}

define private i32 @fcall() {
s.label:
    call void @fcall_0()
    %r = call i32 (i32, ptr, ...) @fcall_1(i32 1, ptr inttoptr(i64 2 to ptr), f32 3)
    ret %r
}

define private void @fvararg(i64 %fmt, ...) {
s.label:
    %vp = alloca i64, i64 32, align 4
    call void @llvm.va_start(ptr %vp)
    %va = va_arg ptr %vp, i32
    ret void
}

define private i32 @fflow() {
s.label:
    br i1 0, label %t.label, label %f.label
t.label:
    %t = add i32 1, 0
    br label %e.label
f.label:
    %f = add i32 0, 0
    br label %e.label
e.label:
    %r = phi i32 [%t, %t.label], [%f, %f.label]
    ret i32 %r
h.label:
   unreachable
}
