import os
import jinja2
import pandas as pd
from bs4 import BeautifulSoup


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
    for country in countries:
        for chart_number in range(0, 8):
            chart_name = "c{}".format(chart_number)
            chart_filename = "charts_interactive/{}_{}.html".format(country["slug"], chart_name)
            if os.path.isfile(chart_filename):
                with open(chart_filename, "r") as chart_html_file:
                    chart_html = chart_html_file.read()
                    soup = BeautifulSoup(chart_html, 'html.parser')
                    body = soup.find('body')
                    chart_contents = body.decode_contents()
                    chart_contents = chart_contents.replace('style="width:100%;height:600px;"', 'style="width:1200px;height:600px;"')
                    chart_contents = chart_contents.replace(
                        '{"viewer":{"width":"100%","height":400,"padding":15,"fill":true},"browser":{"width":"100%","height":400,"padding":40,"fill":true}}',
                        '{"viewer":{"width":"1200","height":600,"padding":15,"fill":false},"browser":{"width":"1200","height":600,"padding":40,"fill":false}}'
                    )
                    country[chart_name] = chart_contents
            else:
                country[chart_name] = "<p><b>No data</b></p>"

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

        output = template.render(country=country)
        html_file = "render_interactive/{}.html".format(country["slug"])
        with open(html_file, "w") as outfile:
            outfile.write(output)
