.class public com/epiphany/SysOutMacros
.super java/lang/Object


.method public <init> : ()V
    .limit stack 1
    .limit locals 1
    aload_0
    invokespecial java/lang/Object <init> ()V
    return
.end method

;; Prints a integer and a newline to standard out.
;; @param 0 I The integer to print.
.method public static println : (I)V
    .limit stack 2
    .limit locals 1
    getstatic java/lang/System out Ljava/io/PrintStream;
    iload_0
    invokevirtual java/io/PrintStream println (I)V
    return
.end method

;; Prints an object and a newline to standard out.
;; @param 0 Ljava/lang/Object; The object to print.
.method public static println : (Ljava/lang/Object;)V
    .limit stack 2
    .limit locals 1
    getstatic java/lang/System out Ljava/io/PrintStream;
    aload_0
    invokevirtual java/io/PrintStream println (Ljava/lang/Object;)V
    return
.end method

;; Prints a string and a newline to standard out.
;; @param 0 Ljava/lang/String; The string to print.
.method public static println : (Ljava/lang/String;)V
    .limit stack 2
    .limit locals 1
    getstatic java/lang/System out Ljava/io/PrintStream;
    aload_0
    invokevirtual java/io/PrintStream println (Ljava/lang/String;)V
    return
.end method

