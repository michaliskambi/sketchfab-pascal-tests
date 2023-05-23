# Using Sketchfab API to search and download glTF (for Castle Game Engine)

Use Sketchfab API to perform a search + download a glTF file. For extra effect, at the end it opens resulting glTF with view3dscene.

Purpose: Sketchfab integration with [Castle Game Engine](https://castle-engine.io/) editor.

## Screenshots

![Screenshot from Sketchab](cthulhu_sketchfab.png)
![Same model downloaded and viewed by view3dscene](cthulhu_view3dscene.png)

## Usage

* Get your Sketchfab token from https://sketchfab.com/settings/password , paste it into `sketchfab_token.inc` file here (this file is ignored by `.gitignore`)

* Build and run the application:

    ```
    castle-engine compile --mode=debug
    ./sketchfab_download
    ```

    Response should be like this:

    ```
    Found 24 models for query: cthulhu
    0 : https://sketchfab.com/3d-models/4737a3b84e00415b9d8bb42ae44285b2
    1 : https://sketchfab.com/3d-models/42002fa6abcc41e18abeb3da9edca096
    .....
    Starting download of model id 4737a3b84e00415b9d8bb42ae44285b2
    Got response, storing in response.json (for debug)
    Downloading model from: https://sketchfab-prod-media.s3.amazonaws.com/archives/....
    Download size: 82.61 MB
    Model downloaded to: model.zip, file size: 82.61 MB
    Model extracted to: model/4737a3b84e00415b9d8bb42ae44285b2
    ```

* Pass a command-line parameter to change the query. It is `cthulhu` by default. Use anything else, this is just a query string for Sketchfab, you can look for anything on Sketchfab. E.g. run

    ```
    ./sketchfab_download castle
    ```

* By default is downloads the first search hit, but you can easily customize it.

    * Click on the list of URLs in console to easily inspect the models.

    * If you want to download a particular model, then comment out

        ```
        ModelID := TSketchfabModel.SearchGetFirst(Query);
        ```

        in code with a simple hardcoded value for your model, e.g.

        ```
        ModelID := '42002fa6abcc41e18abeb3da9edca096';
        ```

## Authors

Michalis Kamburelis and GitHub Copilot. Or the other way around.
