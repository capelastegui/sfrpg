"""Command-line entry point: `uv run sfrpg --help`."""

from __future__ import annotations

from pathlib import Path

import typer

from . import outputs
from .data import repository as repo

app = typer.Typer(help="Build tools for the Square Fireballs RPG book.", no_args_is_help=True)

BUILD_DIR = outputs.ROOT / "build"


@app.command()
def check(strict: bool = typer.Option(False, help="Exit with an error if problems are found.")):
    """Report referential-integrity problems in the CSV data."""
    issues = repo.validate()
    for issue in issues:
        typer.echo(issue)
    unmapped = repo.unmapped_powers()
    if unmapped:
        typer.echo(f"(info) powers not mapped to any build: {', '.join(unmapped)}")
    typer.echo(f"{len(issues)} problem(s) found.")
    if strict and issues:
        raise typer.Exit(1)


@app.command()
def sheet(
    path: Path = typer.Argument(..., exists=True, help="Character sheet YAML file."),
    out: Path = typer.Option(BUILD_DIR / "sheets", help="Output directory."),
    pdf: bool = typer.Option(False, help="Also render to PDF (needs WeasyPrint)."),
):
    """Render a character sheet from a YAML file (see examples/)."""
    try:
        typer.echo(f"Wrote {outputs.write_sheet(path, out, pdf)}")
    except (repo.DataError, RuntimeError) as e:
        typer.echo(f"Error: {e}", err=True)
        raise typer.Exit(1) from e


@app.command()
def pages(out: Path = typer.Option(BUILD_DIR / "pages", help="Output directory.")):
    """Write standalone HTML pages for every class/origin build and monster category."""
    written = outputs.write_pages(out)
    typer.echo(f"Wrote {len(written)} pages to {out}")


@app.command()
def pdf(
    out: Path = typer.Option(BUILD_DIR / "sfrpg.pdf", help="Output PDF path."),
    html_only: bool = typer.Option(False, help="Only write the print HTML (no WeasyPrint)."),
):
    """Build the printable book."""
    if html_only:
        target = out.with_suffix(".html")
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(outputs.print_html(), encoding="utf-8")
        typer.echo(f"Wrote {target}")
        return
    try:
        typer.echo(f"Wrote {outputs.build_pdf(out)}")
    except RuntimeError as e:
        typer.echo(str(e), err=True)
        raise typer.Exit(1) from e


if __name__ == "__main__":
    app()
