.class public com/epiphany/MicroMessagingApp
.super java/lang/Object


.method public <init> : ()V
    .limit stack 1
    .limit locals 1
    aload_0
    invokespecial java/lang/Object <init> ()V
    return
.end method


.method public static main : ([Ljava/lang/String;)V
    .limit stack 6
    .limit locals 1

    new com/epiphany/arg_parse/ArgumentParser
    dup
    invokespecial com/epiphany/arg_parse/ArgumentParser <init> ()V

    dup
    ldc "p"
    aconst_null
    invokevirtual com/epiphany/arg_parse/ArgumentParser addArgument (Ljava/lang/String;Ljava/lang/String;)Lcom/epiphany/arg_parse/Argument;
    pop

    aload_0
    invokevirtual com/epiphany/arg_parse/ArgumentParser parseArgs ([Ljava/lang/String;)Ljava/util/HashMap;
    dup
    invokestatic com/epiphany/SysOutMacros println (Ljava/lang/Object;)V
    ifnull LPrintNull

    return

LPrintNull:
    ldc "null"
    invokestatic com/epiphany/SysOutMacros println (Ljava/lang/String;)V
    return
.end method
