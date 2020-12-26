from pathlib import Path

import bs4
from bs4 import BeautifulSoup


def update_node(node: bs4.element.Tag):
    title_tag = node.findChild("title")
    a_tag = node.findChild("a")
    org_fname = Path(title_tag.string).name.split(".")[0]
    canonical_url = "https://macdavid313.xyz/wiki/" + org_fname + ".html"
    # title_tag.string = org_fname
    a_tag.attrs["xlink:href"] = canonical_url
    a_tag.attrs["target"] = "_self"


def update_all_nodes_urls(fpath: Path):
    with fpath.open("r") as fp:
        bs = BeautifulSoup(fp.read(), features="html.parser")
        for node in bs.find_all("g", class_="node"):
            update_node(node)
    with fpath.open("w") as fp:
        fp.write(bs.prettify())


if __name__ == "__main__":
    fpath = Path(__file__).parent.joinpath("public_html", "static", "img", "graph.svg")
    update_all_nodes_urls(fpath)
