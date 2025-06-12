(defsystem "lem-config"
  :author "Erik P Almaraz"
  :license "MIT"
  :version "0.0.1"
  :description "Lem Configuration."
  :class :package-inferred-system
  :depends-on ("lem"
               "lem-config/source/all")
  :long-description "
Personal Lem configuration, scaffolded as it's own system using ASDF's
package-inferred-system. Includes configurations, 'fixes' as well as code 
base for my own purposes - perhaps to push upstream if and when I can.
")

