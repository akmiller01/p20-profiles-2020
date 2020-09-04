import jinja2
import pandas as pd


def year(num):
    if num is not None:
        return "{:}".format(round(num))
    return "NA"


def people(num):
    if num is not None:
        return "{:,}".format(round(num))
    return "NA"


def dollar(num):
    if num is not None:
        return "${0:,.2f}".format(float(num))
    return "NA"


def roundDollar(num):
    if num is not None:
        return "${0:,}".format(round(num))
    return "NA"


def percent(num):
    if num is not None:
        return "{}%".format(round(num))
    return "NA"


def roundInt(num):
    if num is not None:
        return round(float(num))
    return "NA"


if __name__ == "__main__":
    csv_file = "./data/countries_merged.csv"
    countries_df = pd.read_csv(csv_file, keep_default_na=False, na_values=[""])
    countries_df = countries_df.where(countries_df.notnull(), None)
    countries = countries_df.to_dict('records')
    templateLoader = jinja2.FileSystemLoader(searchpath="./html_template/")
    templateEnv = jinja2.Environment(loader=templateLoader)
    templateEnv.filters['people'] = people
    templateEnv.filters['dollar'] = dollar
    templateEnv.filters['roundDollar'] = roundDollar
    templateEnv.filters['percent'] = percent
    templateEnv.filters['year'] = year
    templateEnv.filters['roundInt'] = roundInt
    TEMPLATE_FILE = "template.html.j2"
    template = templateEnv.get_template(TEMPLATE_FILE)

    output = template.render(countries=countries)
    html_file = "index.html"
    with open(html_file, "w") as outfile:
        outfile.write(output)
