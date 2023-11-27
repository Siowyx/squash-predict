import os
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC


# path to the selenium driver
os.environ['PATH'] += r"C:/Program Files"

# start selenium webdriver
driver = webdriver.Chrome()
driver.get("https://www.psaworldtour.com/tournaments/")

driver.implicitly_wait(10)

seasons = [str(x) + "-" + str(x+1) for x in range(2022, 1991, -1)] + ["1986-1987", "1983-1984"]


for season in seasons:
    season_selector = Select(driver.find_element(By.CLASS_NAME, "select"))
    season_selector.select_by_value(season)

    # wait for the html to render correctly after selecting a season option
    wait = WebDriverWait(driver, 10)
    element = wait.until(EC.element_to_be_clickable((By.CLASS_NAME, "select")))

    # Find all tournament links using their CSS selector within the tours element
    tournament_links = driver.find_elements(By.CSS_SELECTOR, ".name-cell a")

    # Extract href attribute (URL) from each link and write to file
    with open('rawdata/tournament_links_' + season + '.txt', 'w') as file:
        # Write lines to the file one by one
        for link in tournament_links:
            try:
                tournament_url = link.get_attribute("href")
                file.write(tournament_url + "\n")
            except:
                continue


# Close the browser window
driver.quit()

