.class public com/epiphany/arg_parse/Argument
.super java/lang/Object


;; Represents a command-line argument.


.field public final name Ljava/lang/String;
.field public final longName Ljava/lang/String;
;; Whether or not the argument is positional.
.field public final positional Z

;; The value to be returned if the argument is not on the command line. Useless for positional arguments. Can be null.
.field public defaultValue Ljava/lang/Object;
;; The runnable to be executed when the argument is used. Can be null.
.field public action Ljava/lang/Runnable;

.field argumentCount I
.field helpMessage Ljava/lang/String;
.field choices [Ljava/lang/String;
.field metaVariable Ljava/lang/String;


;; Creates a new argument with the given argument names.
;; If the short name starts with a hyphen, it is assumed to be an optional argument. Otherwise, it is positional.
;; The long name is ignored if it is a positional argument.
;;
;; Long names should begin with two hyphens.
;;
;; @param 0 Ljava/lang/String; The short name of the argument.
;; @param 1 Ljava/lang/String; The long name of the argument. Optional.
.method <init> : (Ljava/lang/String;Ljava/lang/String;)V
    .limit stack 3
    .limit locals 3
    aload_0
    invokespecial java/lang/Object <init> ()V

    aload_0
    aload_1
    putfield com/epiphany/arg_parse/Argument name Ljava/lang/String;

    aload_0
    ldc ""
    putfield com/epiphany/arg_parse/Argument helpMessage Ljava/lang/String;

    aload_0
    aconst_null
    putfield com/epiphany/arg_parse/Argument defaultValue Ljava/lang/Object;

    aload_0
    aconst_null
    putfield com/epiphany/arg_parse/Argument action Ljava/lang/Runnable;

    aload_0
    iconst_0
    anewarray java/lang/String
    putfield com/epiphany/arg_parse/Argument choices [Ljava/lang/String;

    ; Tests if the short name represents an optional argument.
    aload_1
    iconst_0
    invokevirtual java/lang/String charAt (I)C
    ldc 45 ; Code for -
    if_icmpne LIsPositional

    aload_0
    aload_2
    putfield com/epiphany/arg_parse/Argument longName Ljava/lang/String;

    aload_0
    iconst_0
    putfield com/epiphany/arg_parse/Argument positional Z

    aload_0
    iconst_0
    putfield com/epiphany/arg_parse/Argument argumentCount I

    ; Gets meta variable from the long name. Or, if it is null, the short name.
    aload_2
    ifnull LLongNameNull

    aload_2
    goto LLongNameNotNull

LLongNameNull:
    aload_1

LLongNameNotNull:
    ; Gets rid of dashes, sets meta variable.
    ldc "-"
    ldc ""
    invokevirtual java/lang/String replaceAll (Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;
    aload_0
    swap
    putfield com/epiphany/arg_parse/Argument metaVariable Ljava/lang/String;

    return

LIsPositional:
    aload_0
    aconst_null
    putfield com/epiphany/arg_parse/Argument longName Ljava/lang/String;

    aload_0
    iconst_1
    putfield com/epiphany/arg_parse/Argument positional Z

    aload_0
    iconst_1
    putfield com/epiphany/arg_parse/Argument argumentCount I

    aload_0
    aload_1
    putfield com/epiphany/arg_parse/Argument metaVariable Ljava/lang/String;

    return
.end method


;; Returns the number of arguments the argument takes in.
.method public getNumberArguments : ()I
    .limit stack 1
    .limit locals 1
    aload_0
    getfield com/epiphany/arg_parse/Argument argumentCount I
    ireturn
.end method


;; Sets the number of arguments this argument should take in.
;; Positional arguments must have at least one argument.
;;
;; @param 0 I The amount of arguments the argument should take in.
;; @return This, for chaining.
;; @throws java/lang/UnsupportedOperationException if the argument is positional and the amount is less than 1, or 0 if optional.
.method public setNumberArguments : (I)Lcom/epiphany/arg_parse/Argument;
    .limit stack 5
    .limit locals 2

    ; Makes sure the minimum number of arguments for positionals are 1, and 0 for optional.
    iload_1
    aload_0
    getfield com/epiphany/arg_parse/Argument positional Z
    if_icmplt LFail

    aload_0
    iload_1
    putfield com/epiphany/arg_parse/Argument argumentCount I

    aload_0
    areturn

LFail:
    new java/lang/StringBuilder
    dup

    aload_0
    getfield com/epiphany/arg_parse/Argument positional Z
    ifne LIsPositional

    ldc "Optional"
    goto LBuildMessage

LIsPositional:
    ldc "Positional"

LBuildMessage:
    invokespecial java/lang/StringBuilder <init> (Ljava/lang/String;)V

    ; Constructs error message.
    ldc " arguments must have a minimum of "
    invokevirtual java/lang/StringBuilder append (Ljava/lang/String;)Ljava/lang/StringBuilder;
    aload_0
    getfield com/epiphany/arg_parse/Argument positional Z
    invokevirtual java/lang/StringBuilder append (I)Ljava/lang/StringBuilder;
    ldc " argument(s)!"
    invokevirtual java/lang/StringBuilder append (Ljava/lang/String;)Ljava/lang/StringBuilder;

    invokevirtual java/lang/StringBuilder toString ()Ljava/lang/String;
    new java/lang/UnsupportedOperationException
    dup
    dup2_x1
    pop2
    invokespecial java/lang/UnsupportedOperationException <init> (Ljava/lang/String;)V

    athrow
.end method


;; Gets the help message. This gets displayed when the user asks for help.
.method public getHelpMessage : ()Ljava/lang/String;
    .limit stack 1
    .limit locals 1
    aload_0
    getfield com/epiphany/arg_parse/Argument helpMessage Ljava/lang/String;
    areturn
.end method


;; Sets the help message. This gets displayed when the user asks for help.
;;
;; @param 0 Ljava/lang/String; The string to set as the help message.
;; @return This, for chaining.
;; @throws Ljava/lang/NullPointerException; If the given message is null.
.method public setHelpMessage : (Ljava/lang/String;)Lcom/epiphany/arg_parse/Argument;
    .limit stack 3
    .limit locals 2

    aload_1
    ifnull LFail

    aload_0
    aload_1
    putfield com/epiphany/arg_parse/Argument helpMessage Ljava/lang/String;

    aload_0
    areturn

LFail:
    new java/lang/NullPointerException
    dup
    ldc "The help message cannot be null!"
    invokespecial java/lang/NullPointerException <init> (Ljava/lang/String;)V

    athrow
.end method


;; Sets the value that is used if the user does not use the argument.
;; Can be null.
;; Useless if the argument is positional.
;;
;; @param 0 Ljava/lang/Object; The object to set as the default value.
;; @return This, for chaining.
.method public setDefaultValue : (Ljava/lang/Object;)Lcom/epiphany/arg_parse/Argument;
    .limit stack 2
    .limit locals 2

    aload_0
    aload_1
    putfield com/epiphany/arg_parse/Argument defaultValue Ljava/lang/Object;

    aload_0
    areturn
.end method


;; Sets the runnable that is to be called when the argument is used.
;; Can be null.
;;
;; @param 0 Ljava/lang/Runnable; The runnable to be called.
;; @return This, for chaining.
.method public setAction : (Ljava/lang/Runnable;)Lcom/epiphany/arg_parse/Argument;
    .limit stack 2
    .limit locals 2

    aload_0
    aload_1
    putfield com/epiphany/arg_parse/Argument action Ljava/lang/Runnable;

    aload_0
    areturn
.end method


;; Gets the whitelist of accepted values for this argument.
.method public getChoices : ()[Ljava/lang/String;
    .limit stack 1
    .limit locals 1
    aload_0
    getfield com/epiphany/arg_parse/Argument choices [Ljava/lang/String;
    areturn
.end method


;; Sets the whitelist of accepted values for this argument.
;; If empty, the user can input any value.
;;
;; @param 0 [Ljava/lang/String; The whitelist of choices the argument will accept.
;; @return This, for chaining.
;; @throws java/lang/NullPointerException If the whitelist of choices is null.
.method public setChoices : ([Ljava/lang/String;)Lcom/epiphany/arg_parse/Argument;
    .limit stack 3
    .limit locals 2

    aload_1
    ifnull LFail

    aload_0
    aload_1
    putfield com/epiphany/arg_parse/Argument choices [Ljava/lang/String;

    aload_0
    areturn

LFail:
    new java/lang/NullPointerException
    dup
    ldc "The choices of an argument cannot be null!"
    invokespecial java/lang/NullPointerException <init> (Ljava/lang/String;)V

    athrow
.end method


;; Gets the meta variable, the name under which the result of parsing will be returned.
.method public getMetaVariable : ()Ljava/lang/String;
    .limit stack 1
    .limit locals 1
    aload_0
    getfield com/epiphany/arg_parse/Argument metaVariable Ljava/lang/String;
    areturn
.end method


;; Sets the meta variable, the name under which the result of parsing will be returned.
;;
;; @param 0 Ljava/lang/String; The new meta variable.
;; @throws java/lang/UnsupportedOperationException If the meta variable is empty.
.method public setMetaVariable : (Ljava/lang/String;)Lcom/epiphany/arg_parse/Argument;
    .limit stack 3
    .limit locals 2

    aload_1
    invokevirtual java/lang/String length ()I
    ifeq LFail

    aload_0
    aload_1
    putfield com/epiphany/arg_parse/Argument metaVariable Ljava/lang/String;

    aload_0
    areturn

LFail:
    new java/lang/UnsupportedOperationException
    dup
    ldc "The meta variable cannot be empty!"
    invokespecial java/lang/UnsupportedOperationException <init> (Ljava/lang/String;)V

    athrow
.end method