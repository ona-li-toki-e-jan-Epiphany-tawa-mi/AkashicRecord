const sourceCodeBox = document.getElementById("srcCod");
const compiledCodeBox = document.getElementById("cmpildCod");
const outputBox = document.getElementById("outpt");
const codeDelayForm = document.getElementById("dly");
const memoryView = document.getElementsByClassName("memryGrdElem");
const currentInsturctionMessage = document.getElementById("currInstrct");

outputBox.rows = 1;
memoryView[0].style.color = "#f0810f";
memoryView[0].style.border = "1px solid #e6df44";

/**
 * Draws stuff.
 */
setInterval(draw, 100);
function draw() {
   sourceCodeBox.cols = Math.floor(window.innerWidth / 10) - 1;
   sourceCodeBox.rows = countLines(sourceCodeBox.value, sourceCodeBox.cols) + 1;
   
   compiledCodeBox.cols = Math.floor(window.innerWidth / 10) - 1;
   compiledCodeBox.rows = countLines(compiledCodeBox.value, compiledCodeBox.cols) + 1;
   
   outputBox.cols = Math.floor(window.innerWidth / 10) - 1;
   
   for (let i = 0; i < memoryView.length; i++) {
      let memoryCellValue = memory[memoryViewPointer + i];
      
      if (memoryCellValue === undefined) {
         memoryView[i].innerText = 0;
         
      } else
         memoryView[i].innerText = memoryCellValue;
   }
}

/**
 * Increases the size of the output box.
 */
function sizeUp() {
   outputBox.rows += 1;
}

/**
 * Decreases the size of the output box.
 */
function sizeDown() {
   if (outputBox.rows > 1)
      outputBox.rows -= 1;
}

/**
 * Counts the number of lines in a string, given the amount of characters each line shoukd have.
 * 
 * @param {string} string The string to count the number of lines in.
 * @param {number} columns The number of characters each line should have.
 *
 * @returns {number} The number of lines.
 */
function countLines(string, columns) {
   let count = 0;
   let charCount = 0;
   
   for (let i = 0; i < string.length; i++) {
      if (string[i] === '\n') {
         count++;
         charCount = 0;
         
      } else {
         charCount++;
      }
      
      if (charCount >= columns) {
         count++;
         charCount = 0;
         
      } 
   }
   
   return count;
}

/**
 * Does some compiling steps to make the interpreter's job easier.
 */
function preInitialization() {
   let code = sourceCodeBox.value;
   let newCode = "";
   
   const operators = ['+', '-', '<', '>', '[', ']', '.', ','];
   
   for (let i = 0; i < code.length; i++) 
      for (let op = 0; op < operators.length; op++) 
         if (code[i] === operators[op]) {
            newCode += operators[op];
         }  
   
   compiledCodeBox.value = newCode;
   reset();
   stop();
}

let instructionPointer = 0;
let memoryPointer = 0;
let memory = [0];
let memoryViewPointer = 0;

let stepper;
let running = false;
let codeDelay = 10;
let waitingForKeyPress = false;

/**
 * Starts the interpreter.
 */
function start() {
   stepper = setInterval(runStep, codeDelay);
   running = true;
}

/**
 * Stops the interpreter.
 */
function stop() {
   clearInterval(stepper);
   running = false;
}

/**
 * Changes the amount of time between every tick.
 */
function changeIntervalTime() {
   if (codeDelayForm.value !== "") {
      let delay = Math.abs(parseInt(codeDelayForm.value));
     
      if (running && delay !== codeDelay) {
         stop();
         codeDelay = delay;
         start();
         
      } else if (!running)
         codeDelay = delay;
   }
}

/**
 * Runs one tick of the brainf*** code.
 */
function runStep() {
   let code = compiledCodeBox.value;
   let currentInstruction = code[instructionPointer];
   
   if (currentInstruction === undefined) {
      currentInsturctionMessage.innerText = "Current Instruction: _";   
   } else
      currentInsturctionMessage.innerText = "Current Instruction: " + currentInstruction;
   
   if (currentInstruction === '+') {
      memory[memoryPointer]++;
      
      if (memory[memoryPointer] > 255)
         memory[memoryPointer] = 0;
      
      instructionPointer++;
      
   } else if (currentInstruction === '-') { 
      memory[memoryPointer]--;
      
      if (memory[memoryPointer] < 0)
         memory[memoryPointer] = 255;
      
      instructionPointer++;
     
   } else if (currentInstruction === '<') {
      memoryView[memoryPointer - memoryViewPointer].style.color = "#e6df44";
      memoryView[memoryPointer - memoryViewPointer].style.border = "1px solid #f0810f";
      
      if (memoryPointer > 0)
         memoryPointer--;
      
      instructionPointer++;
      
      if (memoryViewPointer > memoryPointer)
         memoryViewPointer--;
      
      memoryView[memoryPointer - memoryViewPointer].style.color = "#f0810f";
      memoryView[memoryPointer - memoryViewPointer].style.border = "1px solid #e6df44";
      
   } else if (currentInstruction === '>') {
      if (memory[memoryPointer + 1] === undefined)
         memory[memoryPointer + 1] = 0;
      
      memoryView[memoryPointer - memoryViewPointer].style.color = "#e6df44";
      memoryView[memoryPointer - memoryViewPointer].style.border = "1px solid #f0810f";
      
      memoryPointer++;
      instructionPointer++;
      
      if (memoryViewPointer + memoryView.length - 1 < memoryPointer)
         memoryViewPointer++;
      
      memoryView[memoryPointer - memoryViewPointer].style.color = "#f0810f";
      memoryView[memoryPointer - memoryViewPointer].style.border = "1px solid #e6df44";
      
   } else if (currentInstruction === '.') {
     if (outputBox.rows <= countLines(outputBox.value, outputBox.cols)) 
        outputBox.value = outputBox.value.slice(outputBox.cols, outputBox.value.length);
     
     outputBox.value += String.fromCharCode(memory[memoryPointer]);
     instructionPointer++;
    
   } else if (currentInstruction === ',') {
      waitingForKeyPress = true;
      instructionPointer++;
      
      stop();
      
   } else if (currentInstruction === '[') {
      if (memory[memoryPointer] === 0) {
         let initialInstrucionPointer = instructionPointer;
         
         while (true) {
            instructionPointer++;
            
            if (!running)
               break;
            
            if (instructionPointer > code.length) {
               stop();
               
               console.log("ERROR: unable to find matching ']' for the '[' at postition: " + initialInstrucionPointer);
               
               break;
            }
            
            if (code[instructionPointer] === ']') 
               break;
         }   
      
      } else 
         instructionPointer++;
   
   } else if (currentInstruction === ']') {  
       if (memory[memoryPointer] !== 0) {
         let initialInstrucionPointer = instructionPointer;
         
         while (true) {
            instructionPointer--;
            
            if (!running)
               break;
            
            if (instructionPointer < 0) {
               stop();
               
               console.log("ERROR: unable to find matching '[' for the ']' at postition: " + initialInstrucionPointer);
               
               break;
            }
            
            if (code[instructionPointer] === '[') 
               break;
         }   
      
      } else 
         instructionPointer++;
    
   } else 
      instructionPointer++;
   
   if (instructionPointer > code.length) 
      stop();
}

/**
 * Resets the interpreter.
 */
function reset() {
   memoryView[memoryPointer - memoryViewPointer].style.color = "#e6df44";
   memoryView[memoryPointer - memoryViewPointer].style.border = "1px solid #f0810f";
   
   instructionPointer = 0;
   memoryPointer = 0;
   memory = [0];
   outputBox.value = "";
   running = false;
   clearInterval(stepper);
   waitingForKeyPress = false;
   memoryViewPointer = 0;
   
   memoryView[0].style.color = "#f0810f";
   memoryView[0].style.border = "1px solid #e6df44";
   
   currentInsturctionMessage.innerText = "Current Instruction: _";
}

window.addEventListener("keydown", function(event) {
   if (waitingForKeyPress) {
      memory[memoryPointer] = event.keyCode;
      waitingForKeyPress = false;
      
      start();
   }
});
