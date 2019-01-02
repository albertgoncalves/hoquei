#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from os import environ
from os.path import isfile
from time import sleep

from selenium.webdriver import Chrome
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys

from utils import filename
from utils import seasons


def wait():
    sleep(0.5)


def url(year):
    stem = "https://www.hockey-reference.com/leagues/NHL_{}_games.html"
    return stem.format(year)


def season_xpaths(season):
    if season == "regular":
        suffix = ""
    elif season == "playoffs":
        suffix = "_{}".format(season)

    def season_xpath(xpath):
        return xpath.format(suffix)

    stems = \
        [ '//*[@id="all_games{}"]/div[1]/h2'
        , '//*[@id="all_games{}"]/div[1]/div/ul/li[1]/span'
        , '//*[@id="all_games{}"]/div[1]/div/ul/li[1]/div/ul/li[4]/button'
        , '//*[@id="csv_games{}"]'
        ]

    return list(map(season_xpath, stems))


def scroll_to_xpath(browser, xpath):
    element = browser.find_element(By.XPATH, xpath)
    wait()
    browser.execute_script("arguments[0].scrollIntoView();", element)
    for _ in range(2):
        wait()
        browser.find_element_by_css_selector("body").send_keys(Keys.UP)


def click_xpath(browser, xpath):
    browser.find_element(By.XPATH, xpath).click()
    wait()


def scrape_season(browser, year, season):
    xpaths = season_xpaths(season)

    scroll_to_xpath(browser, xpaths[0])

    for xpath in xpaths[1:]:
        click_xpath(browser, xpath)

    return browser.find_element(By.XPATH, xpaths[-1]).text


def write_season(data, year, season):
    with open(filename(season, year), "w") as f:
        f.write(data)


def main():
    browser = Chrome(environ["chromedriver_path"])

    for year in range(2009, 2019):
        for season in seasons():
            if not isfile(filename(season, year)):
                wait()
                browser.get(url(year))
                data = scrape_season(browser, year, season)
                write_season(data, year, season)

    browser.close()


if __name__ == "__main__":
    main()
