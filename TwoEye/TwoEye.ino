/*
  Blink
  Turns on an LED on for one second, then off for one second, repeatedly.
 
  This example code is in the public domain.
 */
const int leftEye = A0;
const int rightEye = A1;

int leds[8] = { 5, 6, 7, 8, 9, 10, 11, 12 };
int nLeds =  sizeof(leds) / sizeof(*leds);

int EyeVal(int eyePin)
{
  int v = analogRead(eyePin);
  return map(constrain(v, 25, 900), 25, 900, 1, 1000);
}  

// the setup routine runs once when you press reset:
void setup() {                
  Serial.begin(9600);
  // initialize the digital pin as an output.
  for (int i = 0; i < nLeds; ++i)
    pinMode(leds[i], OUTPUT);
  //  pinMode(led, OUTPUT);     
}

// the loop routine runs over and over again forever:
void loop() {
  static int i = 0;
  
  int l = EyeVal(leftEye);
  int r = EyeVal(rightEye);

  Serial.println(l);
  Serial.println(r);
  
  
  
  int j = i;
  i = ++i % nLeds;
  digitalWrite(leds[j], LOW);    // turn the LED off by making the voltage LOW
  
  digitalWrite(leds[i], HIGH);   // turn the LED on (HIGH is the voltage level)
//  Serial.println(i);
  delay(l);               // wait for a second
}

