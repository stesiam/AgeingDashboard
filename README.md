# Ageing Dashboard

[![Docker Image CI](https://github.com/stesiam/AgeingDashboard/actions/workflows/docker-image.yml/badge.svg)](https://github.com/stesiam/AgeingDashboard/actions/workflows/docker-image.yml)

[Web App Link](https://stesiam.shinyapps.io/AgeingDashboard/)

On this repository we have a Web App using [Shiny](https://shiny.posit.co/) framework and [Rstats](https://www.r-project.org/).
You can get the image by running the following command in your terminal:

```bash
docker pull stesiam/ageing-dashboard
```

To run the image:

```bash
docker run -p 3838:3838 stesiam/ageing-dashboard
```

When you run the image, the App will be available at <a href="http://0.0.0.0:3838">http://0.0.0.0:3838</a>

**Note :** In case you get a permission error, consider to use <b><i>sudo</i></b> on the aforementioned commands.

<hr>

📦 **DockerHub Repo:** [ageing-dashboard](https://hub.docker.com/r/stesiam/ageing-dashboard)

💾 **Data Source:**  [Gapminder](https://www.gapminder.org/)
