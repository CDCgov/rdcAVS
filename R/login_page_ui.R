# Authentication page function (Where the app land first) --------

ui_authenticate <- function() {
  div(
    id = "Authenticate",
    style = "
      min-height:100vh;
      display:flex;
      align-items:center;
      justify-content:center;
      padding:24px;
    ",
    div(
      style = "
        width:100%;
        max-width:780px;
        background:rgba(255,255,255,0.95);
        border:1px solid #edebe9;
        border-radius:18px;
        padding:22px;
        box-shadow:0 18px 60px rgba(0,0,0,0.35);
      ",

      Stack(
        tokens = list(childrenGap = 14),
        Stack(
          horizontal = TRUE,
          verticalAlign = "center",
          tokens = list(childrenGap = 12),
          imageOutput("logo", height = "60px"),
          Stack(
            tokens = list(childrenGap = 2),
            Text(
              variant = "xLarge",
              "Portail de création des masques de saisie des campagnes de vaccination en RDC"
            ),
            Text(
              variant = "small",
              styles = list(root = list(color = "#605e5c")),
              ""
            )
          )
        ),

        MessageBar(
          messageBarType = 5,     # tip
          isMultiline = TRUE,
          styles = list(text = list(fontSize = 14, lineHeight = "1.55")),
          Stack(tokens = list(childrenGap = 6),
            Text("Veuillez saisir votre adresse mail, ensuite cliquez sur “S’authentifier”."),
            Text("Cette application ne collecte ni n’enregistre votre mot de passe."),
            Text("Une fenêtre va s’ouvrir : suivez les instructions pour compléter l’authentification.")
          )
        ),
        Separator(),
        Stack(
          horizontal = TRUE,
          wrap = TRUE,
          tokens = list(childrenGap = 12),
          verticalAlign = "end",

          div(
            style = "flex:2; min-width:320px;",
            TextField.shinyInput(
              "email",
              label = "Entrez votre adresse email",
              placeholder = "johndoe@gmail.com",
              iconProps = list(iconName = "Mail"),
              required = TRUE
            )
          ),
          div(
            style = "flex:1; min-width:220px;",
            PrimaryButton.shinyInput(
              "auth_drive",
              text = "S'authentifier",
              iconProps = list(iconName = "AuthenticatorApp"),
              styles = list(root = list(width = "100%", height = "40px"))
            )
          )
        )
      )
    )
  )
}