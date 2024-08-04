from shiny import App, render, ui
import pandas as pd
import io

app_ui = ui.page_fluid(
    ui.panel_title("CSV File Combiner"),
    ui.input_file("file1", "Choose CSV file(s)", multiple=True, accept=[".csv"]),
    ui.output_table("combined_data")
)

def server(input, output, session):
    @output
    @render.table
    def combined_data():
        file_infos = input.file1()
        if not file_infos:
            return None
        
        dfs = []
        for file_info in file_infos:
            content = file_info["datapath"].read()
            df = pd.read_csv(io.StringIO(content.decode("utf-8")))
            dfs.append(df)
        
        combined_df = pd.concat(dfs, ignore_index=True)
        return combined_df.head()

app = App(app_ui, server)
