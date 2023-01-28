from pyftdi.gpio import GpioAsyncController
import time
import sys

if __name__ == "__main__":
    url = "ftdi://ftdi:232:00000000/1"
    gpio = GpioAsyncController()
    gpio.open_from_url(url, direction=255)
    if len(sys.argv) < 2:
        print("Error: Must give bits as argument")
    else:
        bits = int(sys.argv[1])
        print("Writing GPIO pins", bits)
        gpio.write(bits)
        gpio.write(bits)
        gpio.write(bits)
