class Config:
    connection_config = {
        "timeout_millis": 1_500,
        "host": "localhost",
        "port": 44404,
        "wifi_ssid": None,
        "wifi_password": None,
    }
    loop_sleep_millis = 500
    battery_voltage_send_period_millis = 5_000
    deep_sleep_millis = 2_000
    update_timeout = 3_000
