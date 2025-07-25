
<!-- README.md is generated from README.Rmd. Please edit that file -->

This is a shiny app that uses the R packages:

- [`storytelling`](https://github.com/durraniu/storytelling): Create
  fictional stories, illustrations, and narrations using AI models.
  Under the hood, `storytelling` uses {ellmer} and {httr2}

- [`frstore`](https://github.com/Presage-Group/frstore): Perform the
  create, read, update, and delete (CRUD) operations on the Google Cloud
  Firestore database via REST API. `frstore` also uses {httr2}

## How to use the app?

### Use the deployed app

The app is deployed
[here](https://01983eb1-571e-c412-ea19-65f74e04956d.share.connect.posit.cloud/).
If the link does not work, run locally as described below.

### Run locally

Follow these steps to run the app locally:

1.  Clone this repo  
2.  Go to [Firebase website](https://firebase.google.com/) and click “Go
    to console” at the top-right. Log in with your google account  
3.  Create a new Firebase project, register a web app, and then enable
    authentication and Firestore database  
4.  On the **Authentication** page, under the “Sign-in method” tab,
    click “add new provider” and select “Email/Password”. Next, under
    “Users”, add a user by providing email and password. This email
    doesn’t need to be a real email. Under “Settings”, click “Authorized
    domains” and the domain “127.0.0.1”  
5.  Confirm that you see a blank database in the “Data” tab on the
    **Firestore Database** page  
6.  Now click on the gear icon besides the “Project Overview” in the
    sidebar, and select “Project settings”. Scroll to the bottom of the
    page and gather the values of `apiKey`and `projectId`. If you don’t
    see them, you should first add an app (web)  
7.  In your cloned repo folder, create a `.Renviron` file and create the
    env vars `FIREBASE_API_KEY`and `FIREBASE_PROJECT_ID` that should
    have the values of `apiKey` and `projectId` respectively  
8.  Next, get an API token and account ID from Cloudflare by [using
    these
    instructions](https://developers.cloudflare.com/workers-ai/get-started/rest-api/).
    Provide them as `CLOUDFLARE_API_KEY` and `CLOUDFLARE_ACCOUNT_ID` in
    your `.Renviron`  
9.  Similarly, get an API key from [Google AI
    Studio](https://aistudio.google.com/prompts/new_chat) and provide it
    in `.Renviron` as `GOOGLE_API_KEY`  
10. Finally, provide the `EMAIL` and `PASS` env vars in `.Renviron`
    corresponding to the email and password you created for a new user
    on the **Authentication** page. Now, you’re ready to run this app
