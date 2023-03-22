import csv

# Merges all files in the /Dataset repo to one large dataset including city and day.

PATH = "G:/Meine Ablage/Dokumente/THD/Semester/4.Semester/Maschinelles Lernen/Airbnb_prices/Dataset"

cities = ["amsterdam", "athens", "barcelona", "berlin", "budapest", "lisbon", "london", "paris", "rome", "vienna"]
days = ["weekdays", "weekends"]


merged_file = open(f"{PATH}/merged_data.csv", "w")
id_counter = 0
for city in cities:
    for day in days:
        file = open(f"{PATH}/{city}_{day}.csv", "r")
        reader = csv.reader(file)

        # skip the first row if it is not the first file
        if city == "amsterdam" and day == "weekdays":
            headers = next(reader)
            merged_row = ["id", "city", "day"] + headers[1:]
            merged_file.write(",".join(merged_row) + "\n")
        else:
            next(reader)

        # add two new columns to the row and write it to the new file
        for row in reader:
            merged_row = [f"{id_counter}"] + [city] + [day] + row[1:]
            id_counter += 1
            merged_file.write(",".join(merged_row) + "\n")

        file.close()
merged_file.close()
