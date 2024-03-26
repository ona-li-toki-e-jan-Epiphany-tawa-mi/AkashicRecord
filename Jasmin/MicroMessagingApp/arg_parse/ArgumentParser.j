.class public com/epiphany/arg_parse/ArgumentParser
.super java/lang/Object


;; A blatant copy of Python's argparse, translated to Jasmin.


;; Whether or not to add the help command -h (--help). Defaults to true.
.field public addHelp Z

.field private final arguments Ljava/util/ArrayList;


.method public <init> : ()V
    .limit stack 3
    .limit locals 1
    aload_0
    invokespecial java/lang/Object <init> ()V

    aload_0
    new java/util/ArrayList
    dup
    invokespecial java/util/ArrayList <init> ()V
    putfield com/epiphany/arg_parse/ArgumentParser arguments Ljava/util/ArrayList;

    aload_0
    iconst_1
    putfield com/epiphany/arg_parse/ArgumentParser addHelp Z

    return
.end method


;; Adds a new argument to this parser with the given argument names.
;; If the short name starts with a hyphen, it is assumed to be an optional argument. Otherwise, it is positional.
;; Users must use positional arguments.
;; The long name is ignored in positional arguments.
;; Long names should begin with two hyphens.
;; Results are stored in a hashmap, with the keys being the name of the arguments, unless set otherwise.
;;
;; @param 0 Ljava/lang/String; The short name of the argument.
;; @param 1 Ljava/lang/String; The long name of the argument. Can be null
;; @return The created argument.
;; @throws java/lang/UnsupportedOperationException If the short name is "".
.method public addArgument : (Ljava/lang/String;Ljava/lang/String;)Lcom/epiphany/arg_parse/Argument;
    .limit stack 4
    .limit locals 3

    ; Ensures the short name is not empty.
    aload_1
    ldc ""
    invokevirtual java/lang/String equals (Ljava/lang/Object;)Z
    ifne LFailEmpty

    new com/epiphany/arg_parse/Argument
    dup
    aload_1
    aload_2
    invokespecial com/epiphany/arg_parse/Argument <init> (Ljava/lang/String;Ljava/lang/String;)V

    ; Adds argument to the end of the list.
    dup
    aload_0
    getfield com/epiphany/arg_parse/ArgumentParser arguments Ljava/util/ArrayList;
    swap
    invokevirtual java/util/ArrayList add (Ljava/lang/Object;)Z
    pop

    areturn

LFailEmpty:
    new java/lang/UnsupportedOperationException
    dup
    ldc "The short name of an argument cannot be empty!"
    invokespecial java/lang/UnsupportedOperationException <init> (Ljava/lang/String;)V
    athrow
.end method


;;
;;
;; TODO Implement actions.
;; TODO Implement default values for optional arguments.
.method public parseArgs : ([Ljava/lang/String;)Ljava/util/HashMap;
    .limit stack 4
    .limit locals 14

    ; Tests if given array is empty.
    aload_1
    arraylength
    ifeq LEmpty

    ; Tests if internal array is empty.
    aload_0
    getfield com/epiphany/arg_parse/ArgumentParser arguments Ljava/util/ArrayList;
    invokevirtual java/util/ArrayList size ()I
    ifne LNotEmpty

    LEmpty:
        ; Returns a new hashmap.
        new java/util/HashMap
        dup
        invokespecial java/util/HashMap <init> ()V
        areturn

    LNotEmpty:
        ; Creates a new hash map to store the results.
        new java/util/HashMap
        dup
        invokespecial java/util/HashMap <init> ()V
        astore_2

    ; Incrementer for primary loop.
    iconst_0
    istore_3

    ; Gets arguments' array length.
    aload_1
    arraylength
    istore 4

    ; Creates a new, temporary, arguments list, as the parsing method I thought of requires removal.
    new java/util/ArrayList
    dup
    invokespecial java/util/ArrayList <init> ()V
    astore 6

    ; And inserts the arguments into it, optionals going in front, and positionals going in back.
    aload_0
    getfield com/epiphany/arg_parse/ArgumentParser arguments Ljava/util/ArrayList;
    invokevirtual java/util/ArrayList iterator ()Ljava/util/Iterator;

    LInitializeArgsList:
        ; Condition for loop.
        dup
        invokeinterface java/util/Iterator hasNext ()Z 1
        ifeq LFinishArgsListInit

        ; Gets next argument.
        dup
        invokeinterface java/util/Iterator next ()Ljava/lang/Object; 1
        checkcast com/epiphany/arg_parse/Argument
        dup
        getfield com/epiphany/arg_parse/Argument positional Z
        ifne LAddPositionalArgs

            ; Adds optional argument to the front of the list.
            aload 6
            swap
            iconst_0
            swap
            invokevirtual java/util/ArrayList add (ILjava/lang/Object;)V

            goto LInitializeArgsList

        LAddPositionalArgs:
            ; Adds positional argument to the end of the list.
            aload 6
            swap
            invokevirtual java/util/ArrayList add (Ljava/lang/Object;)Z
            pop

            goto LInitializeArgsList

LFinishArgsListInit:
    ; Removes iterator.
    pop

    LForArgsLoop:
        ; Exit condition for loop.
        iload_3
        iload 4
        if_icmpge LExitForLoop

        ; Gets the current argument being read.
        aload_1
        iload_3
        aaload
        astore 5

        ; Controls whether help is added as an argument.
        aload_0
        getfield com/epiphany/arg_parse/ArgumentParser addHelp Z
        ifeq LNotHelpArgument

        ; Tests if the user argument is help.
        aload 5
        ldc "-h"
        invokevirtual java/lang/String equals (Ljava/lang/Object;)Z
        ifeq LIsHelpArgument
        aload 5
        ldc "--help"
        invokevirtual java/lang/String equals (Ljava/lang/Object;)Z
        iconst_1
        if_icmpeq LNotHelpArgument

        LIsHelpArgument:
            ; TODO print out help message.
            iconst_0
            invokestatic java/lang/System exit (I)V

        LNotHelpArgument:
            ; Iterates through added arguments, testing them against the input.
            aload 6
            invokevirtual java/util/ArrayList iterator ()Ljava/util/Iterator;
            astore 7

            LFindArgumentsLoop:
                ; Condition for loop.
                aload 7
                invokeinterface java/util/Iterator hasNext ()Z 1
                ifeq LExitArgumentsLoop

                ; Gets the argument to try.
                aload 7
                invokeinterface java/util/Iterator next ()Ljava/lang/Object; 1
                checkcast com/epiphany/arg_parse/Argument
                astore 8

                ; Tests if the argument is positional or not.
                aload 8
                getfield com/epiphany/arg_parse/Argument positional Z
                iconst_1
                if_icmpeq LPositionalArgument

                    ; Tests for the selected optional argument.
                    aload 5
                    aload 8
                    getfield com/epiphany/arg_parse/Argument name Ljava/lang/String;
                    invokevirtual java/lang/String equals (Ljava/lang/Object;)Z
                    ifeq LIsSelectedArgument

                        goto LFindArgumentsLoop

                    LIsSelectedArgument:
                        ; TODO Implement parsing of optional arguments.

                        goto LFindArgumentsLoop

                LPositionalArgument:
                    ; Gets the number of arguments.
                    aload 8
                    getfield com/epiphany/arg_parse/Argument argumentCount I
                    istore 9

                    ; Incremeted variable.
                    iconst_0
                    istore 10

                    ; Gets meta variable for the argument.
                    aload 8
                    getfield com/epiphany/arg_parse/Argument metaVariable Ljava/lang/String;
                    astore 11

                    ; String to add to hashmap.
                    iload 9
                    anewarray java/lang/String
                    astore 12

                    LArgsCollectorLoop:
                        ; Condition for loop.
                        iload 10
                        iload 9
                        if_icmpge LExitArgsCollectorLoop

                        ; Fails if there are not enough arguments for the argument.
                        iload_3
                        iload 4
                        if_icmpge LNotEnoughArgsError

                        ; Tests if there are any choices to whitelist input arguments.
                        aload 8
                        getfield com/epiphany/arg_parse/Argument choices [Ljava/lang/String;
                        arraylength
                        ifeq LValidArgumentChoice

                            ; Incremented variable for choices .
                            iconst_0
                            istore 13

                            LTestChoicesLoop:
                                ; Condition for loop.
                                iload 13
                                iload 9
                                if_icmpge LInvalidArgumentChoice

                                ; Tests if the given argument is a valid choice.
                                aload 8
                                getfield com/epiphany/arg_parse/Argument choices [Ljava/lang/String;
                                iload 13
                                aaload
                                aload 5
                                invokevirtual java/lang/String equals (Ljava/lang/Object;)Z
                                ifeq LValidArgumentChoice

                                ; Increments counter.
                                iinc 13 1

                        LValidArgumentChoice:
                            ; Adds the string to the output.
                            aload 12
                            iload 10
                            aload 5
                            aastore

                    LInvalidArgumentChoice:
                        ; Adds the resulting arguments under the meta variable to the hashmap.
                        aload_2
                        aload 11
                        aload 12
                        invokevirtual java/util/HashMap put (Ljava/lang/Object;Ljava/lang/Object;)V

                        ; Increments the outer counter.
                        iinc 3 1

                        ; Gets next argument.
                        aload_1
                        iload_3
                        aaload
                        astore 5

                        ; Increments counter.
                        iinc 10 1

                        goto LArgsCollectorLoop

                LExitArgsCollectorLoop:
                    ; Removes the positional argument, as it is only used once.
                    aload 7
                    invokeinterface java/util/Iterator remove ()V 1

                    goto LFindArgumentsLoop

        LExitArgumentsLoop:
            ; Increments counter.
            iinc 3 1

            goto LForArgsLoop

LExitForLoop:
    ; Returns loaded hashmap.
    aload_2
    areturn

LNotEnoughArgsError:
    aconst_null
    areturn

LInvalidChoiceError:
    aconst_null
    areturn
.end method