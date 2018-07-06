import jinja2
from reportlab.lib.pagesizes import letter
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib.units import mm, inch
from reportlab.pdfgen import canvas
from reportlab.platypus import Image, Paragraph, Table
from xml.etree import ElementTree
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont
from reportlab.lib.fonts import addMapping
import re
from PIL import Image as PILImage


########################################################################
class ReportMaker(object):
    """"""

    #----------------------------------------------------------------------
    def __init__(self, countries, template_folder, pdf_file):
        pdfmetrics.registerFont(TTFont('Geomanist', 'fonts/Geomanist-Regular.ttf'))
        pdfmetrics.registerFont(TTFont('Geomanist-Bold', 'fonts/Geomanist-Bold.ttf'))
        pdfmetrics.registerFont(TTFont('Geomanist-Italic', 'fonts/Geomanist-RegularItalic.ttf'))
        addMapping('Geomanist',0,0,'Geomanist')
        addMapping('Geomanist',0,1,'Geomanist-Italic')
        addMapping('Geomanist',1,0,'Geomanist-Bold')

        self.styles = getSampleStyleSheet()
        self.template_folder = template_folder
        templateLoader = jinja2.FileSystemLoader(searchpath="./{}/".format(self.template_folder))
        templateEnv = jinja2.Environment(loader=templateLoader)
        TEMPLATE_FILE = "template.xml.j2"
        template = templateEnv.get_template(TEMPLATE_FILE)

        output = template.render(countries=countries)
        xml_file = "./render/template.xml"
        with open(xml_file, "w") as outfile:
            outfile.write(output)

        self.e = ElementTree.parse(xml_file).getroot()
        self.width, self.height =  int(self.e.getchildren()[0].get("width")), int(self.e.getchildren()[0].get("height"))
        self.c = canvas.Canvas(pdf_file, pagesize=(self.width,self.height),pageCompression=1)
        self.fonts = {}
        for page in self.e.findall("page"):
            for fontspec in page.findall("fontspec"):
                font = {}
                font["size"] = int(fontspec.get("size"))
                font["color"] = fontspec.get("color")
                font["background"] = fontspec.get("background")
                if fontspec.get("indent") is not None:
                    font["indent"] = fontspec.get("indent")
                else:
                    font["indent"] = "0"
                if fontspec.get("padding") is not None:
                    font["padding"] = fontspec.get("padding")
                else:
                    font["padding"] = "0"
                self.fonts[fontspec.get("id")] = font

    #----------------------------------------------------------------------
    def createDocument(self):
        """"""
        for page in self.e.findall("page"):
            self.width, self.height =  int(page.get("width")), int(page.get("height"))
            self.c.setPageSize((self.width,self.height))
            for image in page.findall("image"):
                src = self.template_folder+"/"+image.get("src")
                logo = Image(src)
                logo.drawHeight = int(image.get("height"))
                logo.drawWidth = int(image.get("width"))
                logo.wrapOn(self.c, self.width, self.height)
                logo.drawOn(self.c, *self.coord(int(image.get("left")),int(image.get("top"))+int(image.get("height")) ))
            for text in page.findall("text"):
                if len(text.getchildren())==0:
                    font = self.fonts[text.get("font")]
                    replacement = text.text

                    if text.get("shrink"):
                        fontSize = float(font["size"])
                        height = int(text.get("height"))
                        textLen = float(len(replacement))
                        divisor = max(((textLen/25.0)+(2.0/3.0)),1)
                        fontSizeAdj = int(fontSize / divisor)
                        fontSizeDiff = int(float(fontSize-fontSizeAdj)/2.0)
                        heightAdj = height-fontSizeDiff
                    else:
                        fontSizeAdj = int(font["size"])
                        heightAdj = int(text.get("height"))

                    style = ParagraphStyle(
                        'default',
                        fontName="Geomanist",
                        leading=fontSizeAdj,
                        fontSize=fontSizeAdj,
                        borderPadding = int(font["padding"]),
                        textColor=font["color"],
                        backColor=font["background"],
                        firstLineIndent=int(font["indent"]),
                    )

                    self.createParagraph(replacement, int(text.get("left")), (int(text.get("top"))+heightAdj),style)
                else:
                    innerText = ElementTree.tostring(text.getchildren()[0])
                    font = self.fonts[text.get("font")]
                    replacement = innerText

                    if text.get("shrink"):
                        fontSize = float(font["size"])
                        height = int(text.get("height"))
                        textLen = float(len(replacement))
                        divisor = max(((textLen/25.0)+(2.0/3.0)),1)
                        fontSizeAdj = int(fontSize / divisor)
                        fontSizeDiff = int(float(fontSize-fontSizeAdj)/2.0)
                        heightAdj = height-fontSizeDiff
                    else:
                        fontSizeAdj = int(font["size"])
                        heightAdj = int(text.get("height"))

                    style = ParagraphStyle(
                        'default',
                        fontName="Geomanist",
                        leading=fontSizeAdj,
                        fontSize=fontSizeAdj,
                        borderPadding = int(font["padding"]),
                        textColor=font["color"],
                        backColor=font["background"],
                        firstLineIndent=int(font["indent"]),
                    )

                    self.createParagraph(replacement, int(text.get("left")), (int(text.get("top"))+heightAdj),style)
            for line in page.findall("line"):
                self.c.setDash(int(line.get("on")),int(line.get("off")))
                self.c.setStrokeColor(line.get("color"))
                self.c.line(int(line.get("x1")),self.height-int(line.get("y1")),int(line.get("x2")),self.height-int(line.get("y2")))

            self.c.showPage()

    #----------------------------------------------------------------------
    def coord(self, x, y, unit=1):
        """
        # http://stackoverflow.com/questions/4726011/wrap-text-in-a-table-reportlab
        Helper class to help position flowables in Canvas objects
        """
        x, y = x * unit, self.height -  y * unit
        return x, y

    #----------------------------------------------------------------------
    def createParagraph(self, ptext, x, y, style=None):
        """"""
        if not style:
            style = self.styles["Normal"]
        p = Paragraph(ptext, style=style)
        p.wrapOn(self.c, self.width, self.height)
        p.drawOn(self.c, *self.coord(x, y))

    #----------------------------------------------------------------------
    def savePDF(self):
        """"""
        self.c.save()

#----------------------------------------------------------------------
if __name__ == "__main__":
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
    doc = ReportMaker(countries, "./final_template", "./render/p20_profiles.pdf")
    doc.createDocument()
    doc.savePDF()
