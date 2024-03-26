#include <jni.h>
#include <stdio.h>
#include "com_guitarthang_epiphany_lamestuff_nativecode_windows_WinUSB.h"

JNIEXPORT void JNICALL Java_com_guitarthang_epiphany_lamestuff_nativecode_windows_WinUSB_printfff(JNIEnv *env, jclass thisClass, jstring string) {
    printf("cool!");
    return;
}