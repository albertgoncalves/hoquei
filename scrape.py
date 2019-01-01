#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from os import environ
from time import sleep

from selenium.webdriver import Chrome
from selenium.webdriver.common.by import By


def wait():
    sleep(1)


def click_xpath(browser, xpath):
    browser.find_element(By.XPATH, xpath).click()
    wait()


def scroll_to_y(browser, y):
    browser.execute_script("window.scrollTo(0, {})".format(y))
    wait()


if __name__ == "__main__":
    url = "https://www.hockey-reference.com/leagues/NHL_2019_games.html"
    browser = Chrome(environ["chromedriver_path"])
    browser.get(url)

    scroll_to_y(browser, 500)

    xpaths = \
        [ '//div[@class="section_heading_text"]'
        , '//button[@tip="Export table as <br>suitable for use with excel"]'
        , '//pre[@id="csv_games"]'
        ]

    for xpath in xpaths:
        click_xpath(browser, xpath)

    data = browser.find_element(By.XPATH, xpaths[-1]).text
    browser.close()

    with open("data.csv", "w") as f:
        f.write(data)
