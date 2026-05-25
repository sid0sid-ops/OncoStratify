// =====================================================================
// OncoStratify — Custom JavaScript
// =====================================================================

// == Sidebar Drawer Toggle (Hide / Show) ==
document.addEventListener("click", function (e) {
  const sbToggle = e.target.closest("#btn_sidebar_toggle");
  const sidebar = document.querySelector(".sidebar-left");
  
  if (sbToggle) {
    if (sidebar) {
      sidebar.classList.toggle("collapsed");
      
      // Trigger window resize to adjust ggplot/plots inside container
      window.dispatchEvent(new Event("resize"));
      setTimeout(() => window.dispatchEvent(new Event("resize")), 150);
      setTimeout(() => window.dispatchEvent(new Event("resize")), 300);
    }
  }
});
