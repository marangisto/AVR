const int gnd = A0;
const int xInput = A1;
const int yInput = A2;
const int zInput = A3;
const int vcc = A4;
 
// Raw Ranges:
// initialize to mid-range and allow calibration to
// find the minimum and maximum for each axis
int xRawMin = 512;
int xRawMax = 512;
 
int yRawMin = 512;
int yRawMax = 512;
 
int zRawMin = 512;
int zRawMax = 512;
 
// Take multiple samples to reduce noise
const int sampleSize = 10;
 
void setup() 
{
  pinMode(gnd, OUTPUT);
  digitalWrite(vcc, LOW);
  pinMode(vcc, OUTPUT);
  digitalWrite(vcc, HIGH);
  Serial.begin(9600);
}
 
void loop() 
{
  int xRaw = ReadAxis(xInput);
  int yRaw = ReadAxis(yInput);
  int zRaw = ReadAxis(zInput);
  
  Serial.print(xRaw);
  Serial.print(" ");
  Serial.print(yRaw);
  Serial.print(" ");
  Serial.print(zRaw);
  Serial.print(" ");
  Serial.println();
  delay(200);
}

int ReadAxis(int axisPin)
{
  long reading = 0;
  analogRead(axisPin);
  delay(1);
  for (int i = 0; i < sampleSize; i++)
  {
    reading += analogRead(axisPin);
  }
  return reading/sampleSize;
}

