ruby-ts-mode ruby-mode org-mode

(dd "Rails.logger.info \"debug>>> #{" r "}\"")
(pp "puts \"debug>>> #{" r "}\"")

;; Sidekiq
(qq "queue = Sidekiq::Queue.new('" r "')")
(qeach "queue = Sidekiq::Queue.new('" r "')" n> "queue.each do |job|" n> "klass = job.klass" n> "args = job.args" n "end")
(qpush "Sidekiq::Client.push('class' => " p ", 'args' => [" p "], 'queue' => '" p "')")
