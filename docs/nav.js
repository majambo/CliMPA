// CliMPA navigation: accessible click, touch and keyboard support for dropdowns.
document.addEventListener("DOMContentLoaded", () => {
  const dropdowns = [...document.querySelectorAll(".has-dropdown")];

  function closeDropdown(dropdown) {
    dropdown.classList.remove("open");
    const button = dropdown.querySelector(".dropdown-toggle");
    if (button) button.setAttribute("aria-expanded", "false");
  }

  dropdowns.forEach((dropdown) => {
    const button = dropdown.querySelector(".dropdown-toggle");
    if (!button) return;

    button.addEventListener("click", () => {
      const willOpen = !dropdown.classList.contains("open");
      dropdowns.forEach(closeDropdown);

      if (willOpen) {
        dropdown.classList.add("open");
        button.setAttribute("aria-expanded", "true");
      }
    });
  });

  document.addEventListener("click", (event) => {
    if (!event.target.closest(".has-dropdown")) {
      dropdowns.forEach(closeDropdown);
    }
  });

  document.addEventListener("keydown", (event) => {
    if (event.key === "Escape") {
      dropdowns.forEach(closeDropdown);
    }
  });
});
