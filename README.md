# Using Sketchfab API to download glTF (for Castle Game Engine)

Use Sketchfab API to perform search + download a glTF file.

Purpose: start Sketchfab integration with [Castle Game Engine](https://castle-engine.io/) editor.

Authors: Michalis Kamburelis and GitHub Copilot. Or the other way around.

Usage:

* Get your Sketchfab token from https://sketchfab.com/settings/password , paste it into `sketchfab_token.inc` file here (this file is ignored by `.gitignore`)

* Build and run the application:

    ```
    castle-engine compile --mode=debug
    ./sketchfab_download
    ```

    Response should be like this:

    ```
    Starting download of model id f936c896d628415597b762d4e3944ffc
    Got response, storing in response.json (for debug)
    Downloading model from: https://sketchfab-prod-media.s3.amazonaws.com/....
    Download size: 37.12 MB
    Model downloaded to: model.zip, file size: 37.12 MB
    ```