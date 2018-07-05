import jinja2


templateLoader = jinja2.FileSystemLoader(searchpath="./final_template/")
templateEnv = jinja2.Environment(loader=templateLoader)
TEMPLATE_FILE = "template.xml.j2"
template = templateEnv.get_template(TEMPLATE_FILE)

countries = [
    {
        "longname":"Country long name",
        "name":"Country name",
        "slug":"country-slug",
        "regionslug":"LAC",
        "lowincome":True,
    },
    {
        "longname":"Country long name",
        "name":"Country name",
        "slug":"country-slug",
        "regionslug":"LAC",
        "lowincome":False,
    }
]
output = template.render(countries=countries)

with open("./render/template.xml", "w") as outfile:
    outfile.write(output)
