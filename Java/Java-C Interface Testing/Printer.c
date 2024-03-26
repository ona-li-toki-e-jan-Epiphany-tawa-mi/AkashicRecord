#include <jni.h>
#include <stdio.h>
#include "Printer.h"

JNIEXPORT void JNICALL Java_Printer_printfff(JNIEnv *env, jclass thisClass, jstring string) {
	printf("Awesome Stuff Dude!");
	return;
}