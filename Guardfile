notification :libnotify

ghci_options = [ "-ignore-dot-ghci" ]

guard :haskell, ghci_options: ghci_options do
  watch(%r{test/.+Spec\.l?hs$})
  watch(%r{src/.+\.l?hs$})
end
