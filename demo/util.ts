export const getPermalinkHref = (code: string): string => {
  return `/?code=${window.encodeURIComponent(window.btoa(code.trim()))}`;
}
  