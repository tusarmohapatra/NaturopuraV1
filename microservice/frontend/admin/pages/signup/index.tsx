import React from "react";
import { useEffect, useState } from "react";
import { BrowserWallet } from "@meshsdk/core";
import { useDispatch, useSelector } from 'react-redux';
import { signUp } from '../../action/authActions';
import { useRouter } from "next/router";
const index = () => {
  const [wallets, setWallets] = useState<Array<any>>([]);
  const [formData, setFormData] = useState({
    email: '',
    firstName: '',
    lastName: '',
    contactNumber: '',
    addressLine: '',
    country: '',
    state: '',
    city: '',
    zipCode: '',
    countryCode:'',
    walletName:'',
    type:''
  });
  const [type, setType] = useState<Array<string>>(['admin','farmer','distributors','consultant','agricultural_chemicals','equipment_manufacturers','marketing_agencies','insurance']);
  const user = useSelector((state: any) => state.auth.user);
  const error = useSelector((state: any) => state.auth.error);
  const dispatch = useDispatch();
  const router = useRouter();
  const handleChange = (event: React.ChangeEvent<HTMLInputElement | HTMLSelectElement>) => {
    const { name, value } = event.target;
    setFormData((prevFormData) => ({
      ...prevFormData,
      [name]: value,
    }));
  };

  const handleSubmit = async (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();


    // Perform form submission logic here
    const wallet = await BrowserWallet.enable(formData.walletName);
    const addresses = await wallet.getUsedAddresses();
    const signature = await wallet.signData(addresses[0], 'mesh');
    dispatch(signUp({
      firstName: formData.firstName,
      lastName: formData.lastName,
      email: formData.email,
      isRemember: true,
      signature: signature.signature,
      dialingCode: formData.countryCode,
      address: formData.addressLine,
      addressLine1: formData.addressLine,
      phone: parseInt(formData.contactNumber),
      country: formData.country,
      state: formData.state,
      city: formData.city,
      zipCode: formData.zipCode,
      key: signature.key,
      type: formData.type,
    }));

  };


  useEffect(() => {
    const getBrowser = BrowserWallet.getInstalledWallets();
    console.log(getBrowser, getBrowser, "getBrowser");
    setWallets(getBrowser);
  }, []);

  useEffect(() => {
    if (user) {
      router.push("/");
    }
    console.log(`error`,error );
  }, [error,user]);




  return (
    <div className="container mx-auto py-4">
      <h1 className="text-2xl font-bold mb-4">Registration Form</h1>
      <form onSubmit={handleSubmit} className="w-full max-w-lg">
        <div className="mb-4">
          <label htmlFor="email" className="block font-medium mb-1">
            Email
          </label>
          <input
            type="email"
            id="email"
            name="email"
            value={formData.email}
            onChange={handleChange}
            className="input-field"
            required
          />
        </div>
        <div className="flex mb-4">
          <div className="mr-2 w-1/2">
            <label htmlFor="firstName" className="block font-medium mb-1">
              First Name
            </label>
            <input
              type="text"
              id="firstName"
              name="firstName"
              value={formData.firstName}
              onChange={handleChange}
              className="input-field"
              required
            />
          </div>
          <div className="ml-2 w-1/2">
            <label htmlFor="lastName" className="block font-medium mb-1">
              Last Name
            </label>
            <input
              type="text"
              id="lastName"
              name="lastName"
              value={formData.lastName}
              onChange={handleChange}
              className="input-field"
              required
            />
          </div>
        </div>
        <div className="mb-4">
          <select
            id="countryCode"
            name="countryCode"
            value={formData.countryCode}
            onChange={handleChange}
            className="input-field w-1/4"
            required
          >
            <option value="+1">+1</option>
            <option value="+91">+91</option>
            {/* Add more country code options as needed */}
          </select>
          <label htmlFor="contactNumber" className="block font-medium mb-1">
            Contact Number
          </label>
          <input
            type="tel"
            id="contactNumber"
            name="contactNumber"
            value={formData.contactNumber}
            onChange={handleChange}
            className="input-field"
            required
          />
        </div>
        <div className="mb-4">
        <select
          id="walletName"
          name="walletName"
          className="w-[10rem]"
          required
          value={formData.walletName}
          onChange={handleChange}
        >
          <option>Select a wallet</option>
          {wallets.map((item, index) => {
            return (
              <option key={index} value={item.name}>
                {item.name}
              </option>
            );
          })}
        </select>
        </div>
        <div className="mb-4">
        <select
          id="type"
          name="type"
          className="w-[10rem]"
          required
          value={formData.type}
          onChange={handleChange}
        >
          
          {type.map((item,index)=>{
             return(<option key={index}>{item}</option>)
          })}
          
        </select>
        </div>
        <div className="mb-4">
          <label htmlFor="addressLine" className="block font-medium mb-1">
            Address Line
          </label>
          <input
            type="text"
            id="addressLine"
            name="addressLine"
            value={formData.addressLine}
            onChange={handleChange}
            className="input-field"
            required
          />
        </div>
        <div className="flex mb-4">
          <div className="mr-2 w-1/3">
            <label htmlFor="country" className="block font-medium mb-1">
              Country
            </label>
            <input
              type="text"
              id="country"
              name="country"
              value={formData.country}
              onChange={handleChange}
              className="input-field"
              required
            />
          </div>
          <div className="mx-2 w-1/3">
            <label htmlFor="state" className="block font-medium mb-1">
              State
            </label>
            <input
              type="text"
              id="state"
              name="state"
              value={formData.state}
              onChange={handleChange}
              className="input-field"
              required
            />
          </div>
          <div className="ml-2 w-1/3">
            <label htmlFor="city" className="block font-medium mb-1">
              City
            </label>
            <input
              type="text"
              id="city"
              name="city"
              value={formData.city}
              onChange={handleChange}
              className="input-field"
              required
            />
          </div>
        </div>
        <div className="mb-4">
          <label htmlFor="zipCode" className="block font-medium mb-1">
            ZIP Code
          </label>
          <input
            type="text"
            id="zipCode"
            name="zipCode"
            value={formData.zipCode}
            onChange={handleChange}
            className="input-field"
            required
          />
        </div>
        <button
          type="submit"
          className="bg-blue-500 hover:bg-blue-600 text-white font-medium py-2 px-4 rounded"
        >
          Signup
        </button>
      </form>
    </div>
  );
};

export default index;
