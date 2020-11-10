// The following code is based on the following post:
// https://adminhacks.com/bulma-burger-interactive.html
//
// However, note the typo in the post (id="teamList" doesn't exist)
const toggleBurger = () => {
    let burgerIcon = document.getElementById('burger');
    let dropMenu = document.getElementById('navbarMenu');
    burgerIcon.classList.toggle('is-active');
    dropMenu.classList.toggle('is-active');
  };
