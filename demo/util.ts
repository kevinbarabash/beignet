export const getPermalinkHref = (code: string): string => {
  return `#${window.btoa(code.trim())}`;
};
