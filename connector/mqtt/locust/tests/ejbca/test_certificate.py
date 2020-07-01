"""
Tests for the certification module.
"""

import unittest
from unittest import mock
from unittest.mock import patch, MagicMock, ANY
from src.ejbca.certificate import Certificate

@patch('src.ejbca.certificate.DojotAPI')
@patch('src.ejbca.certificate.RedisClient')
@patch('src.ejbca.certificate.Utils')
@patch('src.ejbca.certificate.crypto')
class TestCertificate(unittest.TestCase):
    """
    Certificate class tests.
    """
    def setUp(self):
        self.thing_id = "testThingID"
        self.jwt = "testJWT"
        self.crt = {'fingerprint' : 'fingerprint', 'pem': 'pem'}

    def test_constructor(self, _mock_crypto, mock_utils, mock_redis, mock_api):
        """
        Certificate class tests.
        """
        mock_api.generate_certificate.return_value = ('fingerprint', 'pem')
        mock_redis.return_value.get_jwt = MagicMock(return_value=self.jwt)
        mock_utils.validate_thing_id.return_value = None

        certificate = Certificate(self.thing_id)

        mock_utils.create_logger.assert_called_once_with("certificate")
        self.assertEqual(certificate.c_name, self.thing_id)
        self.assertEqual(certificate.jwt, self.jwt)

    def test_generate_private_cert(self, mock_crypto, _mock_utils, _mock_redis, mock_api):
        """
        Test generate private cert
        """

        mock_api.generate_certificate.return_value = ('fingerprint', 'pem')
        mock_crypto.dump_privatekey.return_value = MagicMock()
        mock_crypto.dump_privatekey().decode.return_value = 'return-value'
        thing = Certificate(self.thing_id)

        value = thing.generate_private_key()
        self.assertEqual(value, 'return-value')
        mock_crypto.dump_privatekey.assert_called()
        mock_crypto.dump_privatekey().decode.assert_called()
        mock_crypto.dump_privatekey.side_effect = Exception('abc')
        self.assertRaises(Exception, thing.generate_private_key)

    def test_generate_csr(self, mock_crypto, _mock_utils, _mock_redis, mock_api):
        """
        Test generate private csr
        """
        mock_api.generate_certificate.return_value = ('fingerprint', 'pem')
        mock_crypto.dump_certificate_request.return_value = MagicMock()
        mock_crypto.dump_certificate_request().decode.return_value = 'return-value\n'

        thing = Certificate(self.thing_id)
        value = thing.generate_csr()

        self.assertEqual(value, 'return-value')

    def test_generate_certificate(self, _mock_crypto, _mock_utils, mock_redis, mock_api):
        """
        Test generate_certificate()
        """
        mock_api.generate_certificate.return_value = ('fingerprint', 'pem')
        mock_redis.return_value.get_jwt = MagicMock(return_value=self.jwt)

        thing = Certificate(self.thing_id)
        mock_api.generate_certificate.reset_mock()
        cert = thing.generate_certificate()

        self.assertIsNotNone(cert)
        mock_api.generate_certificate.assert_called_once_with(self.jwt, ANY)

    def test_renew_cert(self, _mock_crypto, _mock_utils, mock_redis, mock_api):
        """
        Test generate renew certificate
        """
        mock_api.generate_certificate.return_value = ('fingerprint', 'pem')
        mock_redis.return_value.get_jwt = MagicMock(return_value=self.jwt)

        thing = Certificate(self.thing_id)
        thing.renew_cert()

        mock_api.revoke_certificate.assert_called_once_with(self.jwt, self.crt['fingerprint'])
        self.assertIsNotNone(thing.crt['pem'])

    def test_revoke(self, _mock_crypto, _mock_utils, mock_redis, mock_api):
        """
        Test revoke_certificate
        """
        mock_api.generate_certificate.return_value = ('fingerprint', 'pem')
        mock_redis.return_value.get_jwt = MagicMock(return_value=self.jwt)

        thing = Certificate(self.thing_id)
        thing.revoke_cert()

        mock_api.revoke_certificate.assert_called_once_with(self.jwt, self.crt['fingerprint'])
        self.assertIsNotNone(thing.crt['pem'])


if __name__ == "__main__":
    unittest.main()
