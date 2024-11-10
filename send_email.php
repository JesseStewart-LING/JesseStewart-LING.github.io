<?php
if ($_SERVER["REQUEST_METHOD"] == "POST") {
    $to = "dzesistuert@gmail.com";  // Your email address where the anonymous email will be sent
    $subject = filter_var($_POST['subject'], FILTER_SANITIZE_STRING);
    $message = filter_var($_POST['message'], FILTER_SANITIZE_STRING);

    // Set additional headers
    $headers = "From: no-reply@yourdomain.com\r\n";  // Replace with a valid email from your domain
    $headers .= "Reply-To: no-reply@yourdomain.com\r\n";
    $headers .= "X-Mailer: PHP/" . phpversion();

    // Send the email
    if (mail($to, $subject, $message, $headers)) {
        echo "Email sent successfully.";
    } else {
        echo "Failed to send email.";
    }
} else {
    echo "Invalid request.";
}
?>