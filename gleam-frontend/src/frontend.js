export function raise_alert(message) {
  window.alert(message);
}

// lustre_http needs an absolute URL, so relative paths have to be resolved
// against wherever the frontend is being served from.
export function origin() {
  return window.location.origin;
}
