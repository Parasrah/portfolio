[![Netlify Status](https://api.netlify.com/api/v1/badges/3b803ada-4870-4280-8ea0-870d6848f0c7/deploy-status)](https://app.netlify.com/sites/festive-swirles-53efa5/deploys)

# My Web Portfolio

This is the repository for my personal web portfolio.

## Technologies

Where normally I would build something this insignificant in plain javascript, html and css, I have instead opted to use Elm to highlight this being a technology I am comfortable with.

Aside from Elm, the only other package (at time of writing) being used is [animate.css](https://animate.style/), a popular css library.

## Deployment

While you may notice this repo has bven integrated with Netlify for CI & automated deploys, the domain `https://parasrah.com` is not pointing at one of Netlify's servers. This is
because I am purely using it for CI validation and previews, and the "production" instance is hosted using [NixOps](https://github.com/NixOS/nixops).
