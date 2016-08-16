#include <AVR/UART.h>
#include <AVR/Delay.h>
#include <Arduino/Pins.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef D13 LED;

void setup()
{
    LED::setup();
    UART::setup<9600>();
}

void loop()
{
    static int i = 0;

    LED::toggle();

    char buf[80] = "", *p;

    printf("> ");

    if (!fgets(buf, sizeof(buf), stdin))
        return;

    if (p = strpbrk(buf, "\r\n"))
        *p = 0;

    printf("got '%s'\n", buf);

    int res[8], nres = 0;
    bool inlist = false;

    for (char *p = strtok(buf, " "); p; p = strtok(0, " "))
        if (inlist)
        {
            if (!strcmp(p, "END"))
            {
                inlist = false;
                break;
            }
            else if (nres < sizeof(res) / sizeof(*res))
                res[nres++] = atoi(p);
        }
        else if (!strcmp(p, "COMSTEP"))
            inlist = true;

    if (!inlist)
        for (size_t i = 0; i < nres; ++i)
            printf("%d%s", res[i], i + 1 < nres ? " " : "\n");

    delay_ms(10);
}

int main()
{
    setup();
    for (;;)
        loop();
}

